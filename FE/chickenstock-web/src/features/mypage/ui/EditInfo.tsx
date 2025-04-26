import {
  Card,
  CardContent,
  CardDescription,
  CardFooter,
  CardHeader,
  CardTitle,
} from "@/shared/libs/ui/card";
import { Label } from "@/shared/libs/ui/label";
import { Input } from "@/shared/libs/ui/input";
import { useState } from "react";
import { Button } from "@/shared/libs/ui/button";
const EditInfo = () => {
  const user = {
    email: "user@example.com",
    nickname: "치킨러버",
  };

  const [nickname, setNickname] = useState(user.nickname);
  const [isSubmitting, setIsSubmitting] = useState(false);

  const handleProfileUpdate = (e: React.FormEvent) => {
    e.preventDefault();
    setIsSubmitting(true);

    // 실제로는 API 호출을 통해 업데이트
    setTimeout(() => {
      setNickname(nickname);
      setIsSubmitting(false);
      alert("프로필이 업데이트되었습니다.");
    }, 1000);
  };

  return (
    <Card>
      <CardHeader>
        <CardTitle>기본 정보 수정</CardTitle>
        <CardDescription>프로필 정보를 수정합니다.</CardDescription>
      </CardHeader>
      <form onSubmit={handleProfileUpdate}>
        <CardContent className="space-y-4">
          <div className="space-y-2">
            <Label htmlFor="email">이메일</Label>
            <Input id="email" value={user.email} disabled />
            <p className="text-xs text-muted-foreground">이메일은 변경할 수 없습니다.</p>
          </div>
          <div className="space-y-2">
            <Label htmlFor="nickname">닉네임</Label>
            <Input
              id="nickname"
              value={nickname}
              onChange={(e) => setNickname(e.target.value)}
              placeholder="닉네임을 입력하세요"
            />
          </div>
        </CardContent>
        <CardFooter>
          <Button type="submit" disabled={isSubmitting}>
            {isSubmitting ? "저장 중..." : "저장하기"}
          </Button>
        </CardFooter>
      </form>
    </Card>
  );
};

export default EditInfo;
