/** @type {import('tailwindcss').Config} */
export default {
  content: ["./index.html", "./src/**/*.{js,ts,jsx,tsx}"],
  theme: {
    extend: {
      fontFamily: {
        sans: ["SCDream"],
      },
      colors: {
        primary: {
          DEFAULT: "#FCC023",
          50: "#FFFBEB",
          100: "#FEF3C7",
          200: "#FEE689",
          300: "#FDD141",
          400: "#FCC023",
          500: "#F69E0A",
          600: "#DA7705",
        },
        secondary: {
          DEFAULT: "#FD4141",
          300: "#FF9E9E",
          400: "#FF6666",
          600: "#EB1717",
        },
        gray: {
          DEFAULT: "#8C8C8C",
          0: "#FFFFFF",
          100: "#F5F5F5",
          200: "#E0E0E0",
          300: "#CCCCCC",
          400: "#AFAFAF",
          600: "#6B6B6B",
          700: "#4E4E4E",
          800: "#2C2C2C",
          900: "#1F1F1F",
        },
        danger: "#FF3728",
        warning: "#FFE949",
        info: "#4FA5FF",
        success: "#00C380",
      },
    },

    plugins: [],
  },
};
