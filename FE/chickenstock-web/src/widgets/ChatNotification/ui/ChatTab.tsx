import { useState, useRef, useEffect } from "react";
import { Send, User } from "lucide-react";
import { useChatNotificationStore } from "../model/store";
import { webSocketManager } from "../api/webSocket";
import { cn } from "@/shared/libs/utils";

export default function ChatTab() {
  const [inputMessage, setInputMessage] = useState("");
  const messagesEndRef = useRef<HTMLDivElement>(null);
  const { messages, currentUser, authenticated, connected } = useChatNotificationStore();

  const scrollToBottom = () => {
    messagesEndRef.current?.scrollIntoView({ behavior: "instant" });
  };

  useEffect(() => {
    scrollToBottom();
  }, [messages]);

  const handleSendMessage = (e: React.FormEvent) => {
    e.preventDefault();
    if (inputMessage.trim() && authenticated) {
      webSocketManager.sendChatMessage(inputMessage);
      setInputMessage("");
    }
  };

  return (
    <div className="flex h-full flex-col bg-gray-50">
      {/* 헤더 */}
      <div className="bg-white shadow-sm">
        <div className="px-4 py-3">
          <h3 className="text-center font-medium text-gray-900">실시간 채팅방</h3>
        </div>

        {/* 연결 상태 */}
        <div className="bg-gray-100 px-4 py-1.5 text-center">
          <span
            className={cn(
              "text-xs font-medium",
              connected ? (authenticated ? "text-green-600" : "text-yellow-600") : "text-red-600",
            )}
          >
            {connected ? (authenticated ? "연결됨" : "인증 중...") : "로그인이 필요합니다"}
          </span>
        </div>
      </div>

      {/* 메시지 목록 */}
      <div className="flex-1 overflow-y-auto">
        {messages.length === 0 ? (
          <div className="flex h-full items-center justify-center text-gray-500">
            <p className="text-sm">채팅을 시작해보세요!</p>
          </div>
        ) : (
          <div className="flex min-h-full flex-col justify-end">
            <div className="space-y-3 py-4">
              {messages.map((msg, index) => {
                const isMyMessage = msg.memberId === currentUser?.memberId;
                // 한국 시간으로 변환 (UTC+9)
                const timestamp = msg.timestamp ?? Date.now();
                const koreaDate = new Date(timestamp);

                // 시간 포맷팅 (시:분)
                const hours = koreaDate.getHours().toString().padStart(2, "0");
                const minutes = koreaDate.getMinutes().toString().padStart(2, "0");
                const timeString = `${hours}:${minutes}`;

                return (
                  <div key={index} className={cn("px-4", isMyMessage && "bg-white py-2")}>
                    <div className="flex items-start gap-3">
                      {/* 프로필 */}
                      <div
                        className={cn(
                          "flex size-8 shrink-0 items-center justify-center rounded-full",
                          isMyMessage ? "bg-amber-500" : "bg-gray-300",
                        )}
                      >
                        <User size={16} className="text-white" />
                      </div>

                      {/* 메시지 내용 - 좌측 정렬 */}
                      <div className="min-w-0 flex-1">
                        <div className="mb-1 flex items-baseline gap-2">
                          <span className="text-sm font-medium text-gray-900">{msg.nickname}</span>
                          {isMyMessage && (
                            <span className="text-xs font-medium text-amber-600">나</span>
                          )}
                        </div>
                        <p className="break-words text-left text-sm text-gray-700">{msg.message}</p>
                      </div>

                      {/* 시간 */}
                      <span className="shrink-0 text-xs text-gray-400">{timeString}</span>
                    </div>
                  </div>
                );
              })}
              <div ref={messagesEndRef} />
            </div>
          </div>
        )}
      </div>

      {/* 입력 영역 */}
      <div className="border-t bg-white">
        <form onSubmit={handleSendMessage} className="p-3">
          <div className="flex items-center gap-2">
            <div className="relative flex-1">
              <input
                type="text"
                value={inputMessage}
                onChange={(e) => setInputMessage(e.target.value)}
                placeholder="메시지를 입력하세요..."
                disabled={!authenticated}
                className={cn(
                  "w-full rounded-full border px-4 py-2.5 pr-12",
                  "text-sm placeholder:text-gray-400",
                  "focus:border-amber-500 focus:outline-none",
                  !authenticated && "bg-gray-100",
                )}
              />
              <button
                type="submit"
                disabled={!authenticated || !inputMessage.trim()}
                className={cn(
                  "absolute right-1 top-1/2 -translate-y-1/2",
                  "flex size-9 items-center justify-center",
                  "rounded-full bg-amber-500 text-white",
                  "transition-all hover:bg-amber-600",
                  "disabled:cursor-not-allowed disabled:bg-gray-300 disabled:hover:scale-100",
                )}
              >
                <Send size={18} className="ml-0.5" />
              </button>
            </div>
          </div>
        </form>
      </div>
    </div>
  );
}
