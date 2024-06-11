import { defineConfig } from 'vite'
import solidPlugin from 'vite-plugin-solid'

export default defineConfig({
    clearScreen: false,
    plugins: [solidPlugin()],
    build: {
        rollupOptions: {
            input: './js/App.jsx',
            output: {
                entryFileNames: 'main.js',
            },
        },
    }
})
