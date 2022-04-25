Application {
  Depends {
    name: "cpp"
  }
  cpp.includePaths: ["./include"]
  cpp.cLanguageVersion: "c89"
  name: "μBasic"
  files: [
    "src/main.c",
    "src/micro_io.c",
    "src/micro_basic.c",
    "src/micro_malloc.c",

    "include/micro_io.h",
    "include/micro_basic.h",
    "include/micro_malloc.h",
  ]
}