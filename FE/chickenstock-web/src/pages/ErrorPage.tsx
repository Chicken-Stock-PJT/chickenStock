const ErrorPage = () => {
  return (
    <div className="flex h-screen flex-col items-center justify-center">
      <h1 className="text-4xl font-bold text-danger">404 - Page Not Found</h1>
      <p className="mt-4 text-lg text-gray-900">
        Sorry, the page you are looking for does not exist.
      </p>
    </div>
  );
};

export default ErrorPage;
