load("@rules_haskell//haskell:defs.bzl", "haskell_binary")

# Manual QuickCheck-style example (no external dependencies)
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