import globals from "globals";
import eslint from "@eslint/js";
import tseslint from "typescript-eslint";
import prettierPluginRecommended from "eslint-plugin-prettier/recommended";
import reactHooksPlugin from "eslint-plugin-react-hooks";
import tailwindPlugin from "eslint-plugin-tailwindcss";
import reactRefresh from "eslint-plugin-react-refresh";
import reactPlugin from "eslint-plugin-react";
import { fixupPluginRules } from "@eslint/compat";
// import * as tanstackQueryPlugin from "@tanstack/eslint-plugin-query";

// const tsconfigRootDir = new URL('.', import.meta.url).pathname;

export default tseslint.config(
  { ignores: ["dist", "node_modules", "public"] },
  eslint.configs.recommended,
  ...tseslint.configs.recommendedTypeChecked,
  ...tseslint.configs.stylisticTypeChecked,
  ...tailwindPlugin.configs["flat/recommended"],
  prettierPluginRecommended,
  {
    languageOptions: {
      parserOptions: {
        ecmaFeatures: { jsx: true },
        tsconfigRootDir: import.meta.dirname,
        project: ["./tsconfig.node.json", "./tsconfig.app.json"],
      },
      globals: { ...globals.browser },
    },
  },
  {
    files: ["**/*.{js,mjs,cjs,jsx}"],
    extends: [tseslint.configs.disableTypeChecked],
  },
  {
    plugins: {
      react: reactPlugin,
      "react-hooks": fixupPluginRules(reactHooksPlugin),
      // "@tanstack/query": tanstackQueryPlugin,
      "react-refresh": reactRefresh,
    },
    settings: { react: { version: "detect" } },
    rules: {
      ...reactPlugin.configs["recommended"].rules,
      ...reactPlugin.configs["jsx-runtime"].rules,
      ...reactHooksPlugin.configs.recommended.rules,
      // ...tanstackQueryPlugin.configs.recommended.rules,
      "tailwindcss/no-custom-classname": "off",
      "tailwindcss/no-contradicting-classname": "off",

      "prettier/prettier": [
        "error",
        {
          endOfLine: "auto",
        },
      ],
      "react-refresh/only-export-components": "warn",
      "@typescript-eslint/no-unused-vars": "warn",
      camelcase: "off",
    },
  },
);
