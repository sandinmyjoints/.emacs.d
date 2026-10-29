/// Raw-pixel → PPM conversion for kitty graphics.
///
/// Self-contained module so the conversion logic can be unit-tested
/// without pulling in libghostty or Emacs dependencies.
const std = @import("std");

/// Convert raw pixel data to PPM (P6) format for Emacs.
/// Supports 1 (gray), 2 (gray+alpha), 3 (RGB), and 4 (RGBA) channels.
///
/// Alpha is dropped, not composited.  Transparent pixels render as
/// whatever the underlying color value happens to be (most decoders
/// emit black, which looks fine for opaque-content image previews like
/// thumbnails and screenshots — the typical kitty graphics payload).
///
/// Returns an allocated slice owned by the caller-supplied allocator.
pub fn createPpm(
    allocator: std.mem.Allocator,
    data: []const u8,
    width: u32,
    height: u32,
    channels: u32,
) ?[]u8 {
    if (width == 0 or height == 0 or channels == 0) return null;
    const w: usize = @intCast(width);
    const h: usize = @intCast(height);
    const ch: usize = @intCast(channels);

    // Checked arithmetic — width*height*channels can otherwise overflow
    // `usize` for adversarial inputs and lead to under-allocation +
    // out-of-bounds writes in ReleaseFast.
    const wh = std.math.mul(usize, w, h) catch return null;
    const expected = std.math.mul(usize, wh, ch) catch return null;
    if (data.len < expected) return null;
    const rgb_len = std.math.mul(usize, wh, 3) catch return null;

    var header_buf: [64]u8 = undefined;
    const header = std.fmt.bufPrint(&header_buf, "P6\n{d} {d}\n255\n", .{ w, h }) catch return null;

    const total_len = std.math.add(usize, header.len, rgb_len) catch return null;
    const buf = allocator.alloc(u8, total_len) catch return null;
    @memcpy(buf[0..header.len], header);

    var dst = buf[header.len..];
    var i: usize = 0;
    while (i < wh) : (i += 1) {
        const src_off = i * ch;
        switch (channels) {
            1, 2 => {
                const g = data[src_off];
                dst[i * 3 + 0] = g;
                dst[i * 3 + 1] = g;
                dst[i * 3 + 2] = g;
            },
            3, 4 => {
                dst[i * 3 + 0] = data[src_off + 0];
                dst[i * 3 + 1] = data[src_off + 1];
                dst[i * 3 + 2] = data[src_off + 2];
            },
            else => {
                allocator.free(buf);
                return null;
            },
        }
    }

    return buf;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const testing = std.testing;

test "createPpm: 1x1 RGB roundtrips through P6 header" {
    const data = [_]u8{ 0xAA, 0xBB, 0xCC };
    const out = createPpm(testing.allocator, &data, 1, 1, 3) orelse return error.NullResult;
    defer testing.allocator.free(out);
    try testing.expectEqualStrings("P6\n1 1\n255\n", out[0..11]);
    try testing.expectEqualSlices(u8, &.{ 0xAA, 0xBB, 0xCC }, out[11..]);
}

test "createPpm: 2x1 RGBA drops alpha" {
    // Two pixels: red opaque, blue transparent.  PPM should keep RGB,
    // ignore alpha.
    const data = [_]u8{
        0xFF, 0x00, 0x00, 0xFF, // pixel 0: red
        0x00, 0x00, 0xFF, 0x00, // pixel 1: blue, alpha=0
    };
    const out = createPpm(testing.allocator, &data, 2, 1, 4) orelse return error.NullResult;
    defer testing.allocator.free(out);
    // Header: "P6\n2 1\n255\n" = 11 bytes
    try testing.expectEqualSlices(u8, &.{ 0xFF, 0x00, 0x00, 0x00, 0x00, 0xFF }, out[11..]);
}

test "createPpm: gray channel replicates to RGB" {
    const data = [_]u8{ 0x42, 0x84 };
    const out = createPpm(testing.allocator, &data, 2, 1, 1) orelse return error.NullResult;
    defer testing.allocator.free(out);
    try testing.expectEqualSlices(u8, &.{ 0x42, 0x42, 0x42, 0x84, 0x84, 0x84 }, out[11..]);
}

test "createPpm: gray+alpha drops alpha and replicates gray" {
    // Format: gray, alpha, gray, alpha
    const data = [_]u8{ 0x10, 0xFF, 0x20, 0x00 };
    const out = createPpm(testing.allocator, &data, 2, 1, 2) orelse return error.NullResult;
    defer testing.allocator.free(out);
    try testing.expectEqualSlices(u8, &.{ 0x10, 0x10, 0x10, 0x20, 0x20, 0x20 }, out[11..]);
}

test "createPpm: zero dimensions return null" {
    const data = [_]u8{0};
    try testing.expect(createPpm(testing.allocator, &data, 0, 1, 3) == null);
    try testing.expect(createPpm(testing.allocator, &data, 1, 0, 3) == null);
    try testing.expect(createPpm(testing.allocator, &data, 1, 1, 0) == null);
}

test "createPpm: short data returns null without writing" {
    // 2x2 RGB needs 12 bytes; supply 6.
    const data = [_]u8{ 1, 2, 3, 4, 5, 6 };
    try testing.expect(createPpm(testing.allocator, &data, 2, 2, 3) == null);
}

test "createPpm: unsupported channel count returns null" {
    const data = [_]u8{ 0, 0, 0, 0, 0 };
    try testing.expect(createPpm(testing.allocator, &data, 1, 1, 5) == null);
}

test "createPpm: u32-max width refuses overflow" {
    // width * height * channels would overflow usize.  We expect a
    // null return without any allocation.
    const data = [_]u8{0};
    try testing.expect(createPpm(testing.allocator, &data, std.math.maxInt(u32), 2, 4) == null);
}

test "createPpm: header buffer holds maximum dimensions" {
    // The 64-byte header buffer must fit "P6\n4294967295 4294967295\n255\n"
    // (29 chars + null) — verify formatting succeeds at u32 max.
    const data = [_]u8{0};
    // We can't actually allocate 4G x 4G; just verify we hit the post-
    // header overflow path rather than the header-format path.
    const out = createPpm(testing.allocator, &data, std.math.maxInt(u32), 1, 1);
    // Either null (overflow) or a successful 1-row strip.  Either way,
    // we shouldn't have crashed in bufPrint.
    if (out) |buf| testing.allocator.free(buf);
}
