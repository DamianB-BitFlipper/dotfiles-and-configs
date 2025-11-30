import { tool } from "@opencode-ai/plugin";

export default tool({
  description:
    "Search source code with grep-ast and get not only the matching lines but also the surrounding AST structure: the functions, methods, classes, and control-flow blocks they sit inside. grep-ast walks the project tree by default, respects .gitignore, and understands real programming-language syntax. Use this instead of plain grep when you need code-aware results rather than raw text matches.",

  args: {
    pattern: tool.schema
      .string()
      .min(1, "pattern must not be empty")
      .describe(
        "The regular expression to search for (interpreted by grep-ast; usually a standard regex)."
      ),
    file1: tool.schema
      .string()
      .optional()
      .describe("Optional first filename or glob pattern to restrict the search."),
    file2: tool.schema
      .string()
      .optional()
      .describe("Optional second filename or glob pattern to restrict the search."),
    file3: tool.schema
      .string()
      .optional()
      .describe("Optional third filename or glob pattern to restrict the search."),
    file4: tool.schema
      .string()
      .optional()
      .describe("Optional fourth filename or glob pattern to restrict the search."),
    file5: tool.schema
      .string()
      .optional()
      .describe("Optional fifth filename or glob pattern to restrict the search."),
  },

  async execute({ pattern, file1, file2, file3, file4, file5 }) {
    const cmd = ["~/.config/opencode/tool/grep-ast/venv/bin/grep-ast", pattern];

    for (const file of [file1, file2, file3, file4, file5]) {
      if (file && file.trim().length > 0) {
        cmd.push(file);
      }
    }

    // Make failures from grep-ast visible to the caller instead of throwing a generic process error.
    const { stdout, stderr, exitCode } = await Bun.$`${cmd}`.nothrow().quiet();

    if (exitCode !== 0) {
      const message =
        stderr.toString().trim() || `grep-ast exited with code ${exitCode}`;
      throw new Error(message);
    }

    return stdout.toString().trim();
  },
});
