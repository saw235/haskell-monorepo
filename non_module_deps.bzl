load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@rules_haskell//tools:os_info.bzl", "os_info")

def repositories(*, bzlmod):
    # Some helpers for platform-dependent configuration
    os_info(name = "os_info")

    # For the cat_hs example.
    http_archive(
        name = "zlib.hs",
        build_file_content = """
load("@rules_cc//cc:defs.bzl", "cc_library")
cc_library(
    name = "zlib.hs",
    # Import `:z` as `srcs` to enforce the library name `libz.so`. Otherwise,
    # Bazel would mangle the library name and e.g. Cabal wouldn't recognize it.
    srcs = [":z"],
    hdrs = glob(["*.h"]),
    includes = ["."],
    visibility = ["//visibility:public"],
)
cc_library(
    name = "z",
    srcs = glob(["*.c"]),
    hdrs = glob(["*.h"]),
    copts = select({
        "@bazel_tools//src/conditions:windows": [],
        # Needed to avoid "call to undeclared function" errors [-Wimplicit-function-declaration]
        "//conditions:default": ["-DZ_HAVE_UNISTD_H"],
    }),
)
""",
        sha256 = "9a93b2b7dfdac77ceba5a558a580e74667dd6fede4585b91eefb60f03b72df23",
        strip_prefix = "zlib-1.3.1",
        urls = ["https://github.com/madler/zlib/releases/download/v1.3.1/zlib-1.3.1.tar.gz"],
    )


    # TODO: Remove when tests are run with a ghc version containing Cabal >= 3.10
    # See https://github.com/tweag/rules_haskell/issues/1871
    http_archive(
        name = "Cabal",
        build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
haskell_cabal_library(
    name = "Cabal",
    srcs = glob(["Cabal/**"]),
    verbose = False,
    version = "3.8.1.0",
    visibility = ["//visibility:public"],
)
""",
        sha256 = "b697b558558f351d2704e520e7dcb1f300cd77fea5677d4b2ee71d0b965a4fe9",
        strip_prefix = "cabal-ghc-9.4-paths-module-relocatable",
        urls = ["https://github.com/tweag/cabal/archive/refs/heads/ghc-9.4-paths-module-relocatable.zip"],
    )

    http_archive(
        name = "yaml",
        urls = ["https://hackage.haskell.org/package/yaml-0.11.11.2/yaml-0.11.11.2.tar.gz"],
        build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary", "haskell_cabal_library")
load("@rules_haskell//haskell:defs.bzl", "haskell_library", "haskell_toolchain_library")
haskell_toolchain_library(name = "Cabal", visibility = ["//visibility:private"])
haskell_cabal_library(
    name = "yaml",
    version = "0.11.11.2",
    haddock = False,
    flags = [],
    srcs = glob(["yaml-0.11.11.2/**/*"]),
    deps = ["@stackage//:base", 
        "@stackage//:bytestring", 
        "@stackage//:conduit", 
        "@stackage//:resourcet", 
        "@stackage//:aeson", 
        "@stackage//:attoparsec", 
        "@stackage//:scientific", 
        "@libyaml//:libyaml"
    ],
    setup_deps = [],
    tools = [],
    visibility = ["//visibility:public"],
    cabalopts = ["--ghc-option=-w", "--ghc-option=-optF=-w"],
    verbose = False,
    unique_name = True,
)
        """
    )

    http_archive(
        name = "libyaml",
        urls = ["https://hackage.haskell.org/package/libyaml-0.1.4/libyaml-0.1.4.tar.gz"],
        integrity = "sha256-1il+ybHrtOS1gKQ1fYQNCKb1PhOhDAxT7nS5c7um+hM=",
        strip_prefix = "libyaml-0.1.4",
        build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary", "haskell_cabal_library")
load("@rules_haskell//haskell:defs.bzl", "haskell_library", "haskell_toolchain_library")
haskell_toolchain_library(name = "Cabal", visibility = ["//visibility:private"])
haskell_cabal_library(
    name = "libyaml",
    version = "0.1.4",
    haddock = False,
    flags = ["-system-libyaml"],
    srcs = glob(["libyaml-0.1.4/**/*"]),
    deps = ["@stackage//:base", "@stackage//:conduit", "@stackage//:resourcet", "@stackage//:libyaml-clib"],
    tools = [],
    visibility = ["//visibility:public"],
    cabalopts = ["--ghc-option=-w", "--ghc-option=-optF=-w"],
    verbose = False,
    unique_name = True,
)
        """
    )

    http_archive(
        name = "libyaml-clib",
        urls = ["https://hackage.haskell.org/package/libyaml-clib-0.2.5/libyaml-clib-0.2.5.tar.gz"],
        integrity = "sha256-BFJrxU5fz0ccK9IoVOa1GeFipuPWVOhmzYhiEDxyUUk=",
        build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary", "haskell_cabal_library")
load("@rules_haskell//haskell:defs.bzl", "haskell_library", "haskell_toolchain_library")
haskell_toolchain_library(name = "Cabal", visibility = ["//visibility:private"])
haskell_cabal_library(
    name = "libyaml-clib",
    version = "0.2.5",
    haddock = False,
    flags = [],
    srcs = glob(["libyaml-clib-0.2.5/**/*"]),
    tools = [],
    visibility = ["//visibility:public"],
    cabalopts = ["--ghc-option=-w", "--ghc-option=-optF=-w"],
    verbose = False,
    unique_name = True,
)
    """
    )
def _non_module_deps_impl(_ctx):
    repositories(bzlmod = True)

non_module_deps = module_extension(
    implementation = _non_module_deps_impl,
)