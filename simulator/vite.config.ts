import { defineConfig } from "vite";

export default defineConfig({
  base: "dam4g",
  resolve: {
    alias: {
      "@": __dirname
    }
  }
});