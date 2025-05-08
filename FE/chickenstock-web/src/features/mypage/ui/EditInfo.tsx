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
import { useAuthStore } from "@/shared/store/auth";
import { useUpdateNickname } from "../model/queries";

const EditInfo = () => {
  const { simpleProfile } = useAuthStore();
  const [nickname, setNickname] = useState<string>(simpleProfile?.nickname ?? "");
  const [isSubmitting, setIsSubmitting] = useState(false);
  const { mutateAsync: updateNickname } = useUpdateNickname();

  const handleProfileUpdate = async (e: React.FormEvent) => {
    e.preventDefault();
    setIsSubmitting(true);
    try {
      await updateNickname(nickname);
      alert("프로필이 업데이트되었습니다.");
    } catch (error) {
      // 에러는 useUpdateNickname 훅에서 처리됨
    } finally {
      setIsSubmitting(false);
    }
  };

  return (
    <Card>
      <CardHeader>
        <CardTitle>기본 정보 수정</CardTitle>
        <CardDescription>프로필 정보를 수정합니다.</CardDescription>
      </CardHeader>
      <form onSubmit={(e) => void handleProfileUpdate(e)}>
        <CardContent className="space-y-4">
          {/* <div className="space-y-2">
            <Label htmlFor="email">이메일</Label>
            <Input id="email" value={user.email} disabled />
            <p className="text-xs text-muted-foreground">이메일은 변경할 수 없습니다.</p>
          </div> */}
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
