const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const test_step = b.step("test", "Run all tests in src folder");

    addTestsFromDir(b, test_step, "src", target, optimize);
}

fn addTestsFromDir(
    b: *std.Build,
    test_step: *std.Build.Step,
    dir_path: []const u8,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
) void {
    const allocator = b.allocator;

    // ディレクトリを開く
    var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch |err| {
        std.debug.print("Failed to open directory '{s}': {}\n", .{ dir_path, err });
        return;
    };
    defer dir.close();

    // ディレクトリ内を反復処理
    var iterator = dir.iterate();
    while (iterator.next() catch null) |entry| {
        const full_path = std.fmt.allocPrint(allocator, "{s}/{s}", .{ dir_path, entry.name }) catch continue;

        switch (entry.kind) {
            .file => {
                // .zigファイルの場合、テストを追加
                if (std.mem.endsWith(u8, entry.name, ".zig")) {
                    const test_exe = b.addTest(.{
                        .root_source_file = b.path(full_path),
                        .target = target,
                        .optimize = optimize,
                    });

                    const run_test = b.addRunArtifact(test_exe);
                    test_step.dependOn(&run_test.step);

                    std.debug.print("Added test: {s}\n", .{full_path});
                }
            },
            .directory => {
                // サブディレクトリの場合、再帰的に処理
                addTestsFromDir(b, test_step, full_path, target, optimize);
            },
            else => {},
        }
    }
}
