import { queryClient } from "@/shared/api/queryClient";
import { Query, QueryClientProvider as TanStackQueryClientProvider } from "@tanstack/react-query";
import { ReactQueryDevtools } from "@tanstack/react-query-devtools";
import { AxiosError } from "axios";
import { PropsWithChildren } from "react";

export const QueryClientProvider = ({ children }: PropsWithChildren) => {
  return (
    <TanStackQueryClientProvider client={queryClient}>
      {children}
      <ReactQueryDevtools
        errorTypes={[
          {
            name: "Error",
            initializer: errorInitializer(new Error("Error message")),
          },
          {
            name: "Axios Error",
            initializer: errorInitializer(new AxiosError("Axios error")),
          },
        ]}
      />
    </TanStackQueryClientProvider>
  );
};

function errorInitializer(error: Error) {
  return (query: Query) => {
    query.reset();
    return error;
  };
}
