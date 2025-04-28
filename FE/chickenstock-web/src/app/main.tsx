import { StrictMode } from "react";
import { createRoot } from "react-dom/client";
import "./styles/index.css";
import App from "./App.tsx";
import { QueryClientProvider } from "./providers/index.tsx";

async function prepare() {
  if (process.env.NODE_ENV === "development") {
    const { worker } = await import("../shared/libs/mocks/browser");
    return worker.start();
  }
  return Promise.resolve();
}

void prepare().then(() => {
  createRoot(document.getElementById("root")!).render(
    <StrictMode>
      <QueryClientProvider>
        <App />
      </QueryClientProvider>
    </StrictMode>,
  );
});
