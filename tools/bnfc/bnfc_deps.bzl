"""Module extension for BNFC dependencies."""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def _bnfc_binary_repo_impl(repository_ctx):
    """Implementation for BNFC binary repository."""
    if repository_ctx.attr.platform == "windows":
        url = "https://bnfc.digitalgrammars.com/download/bnfc-2.9.5-win.exe"
        filename = "bnfc.exe"
        sha256 = "dfc08bfe0a84bc50ef7a424d039790fcb9a0226f2c992f9d2a340775b37d6bac"
    else:
        url = "https://bnfc.digitalgrammars.com/download/bnfc-2.9.5-linux-x86_64.binary"
        filename = "bnfc"
        sha256 = "c250c3310a6d0345830236e3fa37254901ce725560a9e829b61315f1a901ade5"
    
    repository_ctx.download(
        url = url,
        output = filename,
        executable = True,
        sha256 = sha256,
    )
    
    repository_ctx.file("BUILD.bazel", """
sh_binary(
    name = "bnfc_tool",
    srcs = ["{}"],
    visibility = ["//visibility:public"],
)
""".format(filename))

_bnfc_binary_repo = repository_rule(
    implementation = _bnfc_binary_repo_impl,
    attrs = {
        "platform": attr.string(mandatory = True),
    },
)

def _bnfc_deps_impl(module_ctx):
    """Implementation of the bnfc_deps module extension."""
    
    _bnfc_binary_repo(
        name = "bnfc_linux",
        platform = "linux",
    )
    
    _bnfc_binary_repo(
        name = "bnfc_windows", 
        platform = "windows",
    )

bnfc_deps = module_extension(
    implementation = _bnfc_deps_impl,
)