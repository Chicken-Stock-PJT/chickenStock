import StockListHeader from "../features/stocks/list/StockListHeader";
import StockListIndex from "../features/stocks/list/StockListIndex";
import StockListItem from "../features/stocks/list/StockListItem";

const StockList = () => {
  return (
    <div>
      <div>
        <StockListHeader />
      </div>
      <div>
        <StockListIndex />
      </div>
      <div>
        <StockListItem />
        <StockListItem />
        <StockListItem />
        <StockListItem />
        <StockListItem />
        <StockListItem />
      </div>
    </div>
  );
};
export default StockList;
