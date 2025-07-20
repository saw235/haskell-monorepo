load("@rules_haskell//haskell:defs.bzl", "haskell_binary", "haskell_test")

# Manual QuickCheck-style example as a test
haskell_test(
    name = "quickcheck-example-test",
    srcs = ["QuickCheckExample.hs"],
    deps = [
        "//:base",
        "@stackage//:random",
    ],
    visibility = ["//visibility:public"],
)

# Keep the binary version for manual execution
haskell_binary(
    name = "quickcheck-example",
    srcs = ["QuickCheckExample.hs"],
    deps = [
        "//:base",
        "@stackage//:random",
    ],
    visibility = ["//visibility:public"],
)

# Real QuickCheck example using the actual library
haskell_binary(
    name = "real-quickcheck-example",
    srcs = ["RealQuickCheckExample.hs"],
    deps = [
        "//:base",
        "@stackage//:QuickCheck",
    ],
    visibility = ["//visibility:public"],
) 