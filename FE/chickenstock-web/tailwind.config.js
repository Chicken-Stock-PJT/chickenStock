/** @type {import('tailwindcss').Config} */
export default {
    darkMode: ["class"],
    content: ["./index.html", "./src/**/*.{js,ts,jsx,tsx}"],
  theme: {
  	extend: {
  		fontFamily: {
  			sans: [
  				'SCDream'
  			]
  		},
  		colors: {
  			primary: {
  				'50': '#FFFBEB',
  				'100': '#FEF3C7',
  				'200': '#FEE689',
  				'300': '#FDD141',
  				'400': '#FCC023',
  				'500': '#F69E0A',
  				'600': '#DA7705',
  				DEFAULT: 'hsl(var(--primary))',
  				foreground: 'hsl(var(--primary-foreground))'
  			},
  			secondary: {
  				'300': '#FF9E9E',
  				'400': '#FF6666',
  				'600': '#EB1717',
  				DEFAULT: 'hsl(var(--secondary))',
  				foreground: 'hsl(var(--secondary-foreground))'
  			},
  			gray: {
  				'0': '#FFFFFF',
  				'100': '#F5F5F5',
  				'200': '#E0E0E0',
  				'300': '#CCCCCC',
  				'400': '#AFAFAF',
  				'600': '#6B6B6B',
  				'700': '#4E4E4E',
  				'800': '#2C2C2C',
  				'900': '#1F1F1F',
  				DEFAULT: '#8C8C8C'
  			},
  			chart: {
  				'1': 'hsl(var(--chart-1))',
  				'2': 'hsl(var(--chart-2))',
  				'3': 'hsl(var(--chart-3))',
  				'4': 'hsl(var(--chart-4))',
  				'5': 'hsl(var(--chart-5))',
  				red: '#FD4141',
  				blue: '#4170FD'
  			},
  			danger: '#FF3728',
  			warning: '#FFE949',
  			info: '#4FA5FF',
  			success: '#00C380',
  			background: 'hsl(var(--background))',
  			foreground: 'hsl(var(--foreground))',
  			card: {
  				DEFAULT: 'hsl(var(--card))',
  				foreground: 'hsl(var(--card-foreground))'
  			},
  			popover: {
  				DEFAULT: 'hsl(var(--popover))',
  				foreground: 'hsl(var(--popover-foreground))'
  			},
  			muted: {
  				DEFAULT: 'hsl(var(--muted))',
  				foreground: 'hsl(var(--muted-foreground))'
  			},
  			accent: {
  				DEFAULT: 'hsl(var(--accent))',
  				foreground: 'hsl(var(--accent-foreground))'
  			},
  			destructive: {
  				DEFAULT: 'hsl(var(--destructive))',
  				foreground: 'hsl(var(--destructive-foreground))'
  			},
  			border: 'hsl(var(--border))',
  			input: 'hsl(var(--input))',
  			ring: 'hsl(var(--ring))'
  		},
  		borderRadius: {
  			lg: 'var(--radius)',
  			md: 'calc(var(--radius) - 2px)',
  			sm: 'calc(var(--radius) - 4px)'
  		}
  	},
  	plugins: []
  },
    plugins: [require("tailwindcss-animate")]
};
