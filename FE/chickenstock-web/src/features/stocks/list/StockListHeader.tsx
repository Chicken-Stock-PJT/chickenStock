const StockListHeader = () => {
  return (
    <div>
      <h1 className="text-4xl font-[600] text-left">실시간 순위</h1>
      <div className="flex items-center justify-between p-4 border-b">
        <div>필터1</div>
        <div>필터2</div>
        <div>필터3</div>
        <div>필터4</div>
      </div>
    </div>
  );
};

export default StockListHeader;
