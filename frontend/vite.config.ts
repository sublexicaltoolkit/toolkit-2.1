import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

// https://vite.dev/config/
export default defineConfig({
  plugins: [react()],
  // The CSV data lives in ../data, outside the frontend folder
  server: {
    fs: { allow: ['..'] },
  },
})
