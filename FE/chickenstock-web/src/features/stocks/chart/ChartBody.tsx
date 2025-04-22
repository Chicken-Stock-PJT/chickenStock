const ChartBody = () => {
  return (
    <div className="chart-body">
      <div className="chart-body__chart">
        {/* Chart will be rendered here */}
      </div>
      <div className="chart-body__info">
        <h1 className="text-3xl font-bold text-gray-800">Stock Price Chart</h1>
        <p className="mt-2 text-gray-600">
          Visualize the stock price trends over time.
        </p>
      </div>
    </div>
  );
};

export default ChartBody;
