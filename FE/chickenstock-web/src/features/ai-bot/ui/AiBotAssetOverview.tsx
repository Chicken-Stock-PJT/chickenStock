import { Card, CardHeader, CardTitle, CardContent } from "@/shared/libs/ui/card";

const AiBotAssetOverview = () => {
  return (
    <div className="flex flex-col gap-2">
      <Card>
        <CardHeader>
          <CardTitle>총 자산</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-2xl font-bold">얼마원</div>
        </CardContent>
      </Card>
      <Card>
        <CardHeader>
          <CardTitle>금일 수익</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-2xl font-bold">얼마원</div>
        </CardContent>
      </Card>
      <Card>
        <CardHeader>
          <CardTitle>예수금</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-2xl font-bold">얼마원</div>
        </CardContent>
      </Card>
      <Card>
        <CardHeader>
          <CardTitle>매수 가능 금액</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-2xl font-bold">얼마원</div>
        </CardContent>
      </Card>
    </div>
  );
};

export default AiBotAssetOverview;
