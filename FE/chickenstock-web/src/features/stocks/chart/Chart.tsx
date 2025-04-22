import ChartBody from "./ChartBody";
import ChartHeader from "./ChartHeader";

const Chart = () => {
  return (
    <div className="w-full h-full border rounded-lg bg-gray-100 flex flex-col gap-4 p-4">
      <ChartHeader />
      <ChartBody />
    </div>
  );
};

export default Chart;
