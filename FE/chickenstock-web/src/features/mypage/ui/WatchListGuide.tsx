import { Card, CardContent, CardHeader, CardTitle } from "@/shared/libs/ui/card";
import { Heart, Search } from "lucide-react";

const WatchListGuide = () => {
  return (
    <Card>
      <CardHeader>
        <CardTitle>관심종목 추가 방법</CardTitle>
      </CardHeader>
      <CardContent>
        <div className="space-y-4">
          <div className="flex items-start gap-4">
            <div className="rounded-full bg-primary-100 p-2">
              <Heart className="size-5 text-primary-500" />
            </div>
            <div>
              <h3 className="font-medium">종목 상세 페이지에서 추가하기</h3>
              <p className="text-sm text-muted-foreground">
                종목 상세 페이지에서 하트 아이콘을 클릭하여 관심종목에 추가할 수 있습니다.
              </p>
            </div>
          </div>
          <div className="flex items-start gap-4">
            <div className="rounded-full bg-primary-100 p-2">
              <Search className="size-5 text-primary-500" />
            </div>
            <div>
              <h3 className="font-medium">종목 검색 후 추가하기</h3>
              <p className="text-sm text-muted-foreground">
                상단 검색창에서 종목을 검색한 후 관심종목에 추가할 수 있습니다.
              </p>
            </div>
          </div>
        </div>
      </CardContent>
    </Card>
  );
};

export default WatchListGuide;
