import { Heart } from "lucide-react";
import { useState } from "react";
import { useNavigate } from "react-router-dom";

const StockListItem = () => {
  const navigate = useNavigate();
  const [like, setLike] = useState<boolean>(false);

  return (
    <div
      className="flex items-center justify-between p-4 border-b"
      // onClick={() => navigate("1")}
    >
      <div className="flex items-center gap-6 text-left">
        <Heart
          className="cursor-pointer"
          fill={like ? "red" : "none"}
          stroke="red"
          strokeWidth={like ? 0 : 1}
          onClick={() => void setLike(true)}
        />
        <div>
          <h2 className="text-lg font-bold">Stock Name</h2>
          <p className="text-gray-500">Stock Code</p>
        </div>
      </div>
      <div className="flex items-center">
        <span className="text-green-500 font-bold">+5.00%</span>
      </div>
    </div>
  );
};

export default StockListItem;
