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
import { updatePassword } from "../api";

const EditPassword = () => {
  const [currentPassword, setCurrentPassword] = useState("");
  const [newPassword, setNewPassword] = useState("");
  const [checkPassword, setCheckPassword] = useState("");
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [error, setError] = useState<string>("");

  const handlePasswordUpdate = async (e: React.FormEvent) => {
    e.preventDefault();
    setError("");
    setIsSubmitting(true);

    // 비밀번호 유효성 검사
    if (newPassword !== checkPassword) {
      setError("새 비밀번호와 확인 비밀번호가 일치하지 않습니다.");
      setIsSubmitting(false);
      return;
    }

    await updatePassword({ currentPassword, newPassword, checkPassword });
    // alert("비밀번호가 변경되었습니다.");
    setIsSubmitting(false);
    setCurrentPassword("");
    setNewPassword("");
    setCheckPassword("");
  };

  return (
    <Card>
      <CardHeader>
        <CardTitle>비밀번호 변경</CardTitle>
        <CardDescription>계정 비밀번호를 변경합니다.</CardDescription>
      </CardHeader>
      <form onSubmit={(e) => void handlePasswordUpdate(e)}>
        <CardContent className="space-y-4">
          <div className="space-y-2">
            <Label htmlFor="current-password">현재 비밀번호</Label>
            <Input
              id="current-password"
              type="password"
              value={currentPassword}
              onChange={(e) => setCurrentPassword(e.target.value)}
              placeholder="현재 비밀번호를 입력하세요"
            />
          </div>
          <div className="space-y-2">
            <Label htmlFor="new-password">새 비밀번호</Label>
            <Input
              id="new-password"
              type="password"
              value={newPassword}
              onChange={(e) => setNewPassword(e.target.value)}
              placeholder="새 비밀번호를 입력하세요"
            />
            <p className="text-xs text-muted-foreground">
              비밀번호는 8자 이상이어야 하며, 문자, 숫자, 특수문자를 포함해야 합니다.
            </p>
          </div>
          <div className="space-y-2">
            <Label htmlFor="confirm-password">비밀번호 확인</Label>
            <Input
              id="confirm-password"
              type="password"
              value={checkPassword}
              onChange={(e) => setCheckPassword(e.target.value)}
              placeholder="새 비밀번호를 다시 입력하세요"
            />
          </div>
          {error && <div className="text-sm text-red-500">{error}</div>}
        </CardContent>
        <CardFooter>
          <Button type="submit" disabled={isSubmitting}>
            {isSubmitting ? "변경 중..." : "비밀번호 변경"}
          </Button>
        </CardFooter>
      </form>
    </Card>
  );
};

export default EditPassword;
