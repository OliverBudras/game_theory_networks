import { defineConfig } from 'vite'
import path from 'path'

export default defineConfig({
  root: '.',      // 'docs' is already root
  base: './',     // relative paths for GitHub Pages
  build: {
    outDir: 'dist',   // output folder
    emptyOutDir: true,
    rollupOptions: {
      input: {
        cluster: path.resolve(__dirname, 'cluster/index.html'),
        field: path.resolve(__dirname, 'field/index.html'),
      },
    },
  },
})
