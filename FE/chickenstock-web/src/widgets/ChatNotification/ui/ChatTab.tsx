import { useState, useRef, useEffect } from "react";
import { Send, User } from "lucide-react";
import { useChatNotificationStore } from "../model/store";
import { webSocketManager } from "../api/webSocket";
import { cn } from "@/shared/libs/utils";

export default function ChatTab() {
  const [inputMessage, setInputMessage] = useState("");
  const messagesEndRef = useRef<HTMLDivElement>(null);
  const { messages, currentUser, authenticated, connected } = useChatNotificationStore();

  // 새 메시지가 올 때 자동 스크롤
  const scrollToBottom = () => {
    messagesEndRef.current?.scrollIntoView({ behavior: "smooth" });
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
    <div className="flex h-full flex-col">
      {/* 연결 상태 표시 */}
      <div className="bg-gray-50 px-4 py-2 text-center text-xs">
        {connected ? (
          authenticated ? (
            <span className="text-green-600">● 연결됨</span>
          ) : (
            <span className="text-yellow-600">● 인증 중...</span>
          )
        ) : (
          <span className="text-red-600">● 연결 끊김</span>
        )}
      </div>

      {/* 메시지 목록 */}
      <div className="flex-1 overflow-y-auto px-4 py-3">
        {messages.length === 0 ? (
          <div className="text-center text-gray-500">채팅을 시작해보세요!</div>
        ) : (
          <div className="space-y-1">
            {messages.map((msg, index) => {
              const isMyMessage = msg.memberId === currentUser?.memberId;
              return (
                <div key={index} className="group">
                  <div
                    className={cn(
                      "flex items-start gap-3 px-3 py-2 rounded-lg",
                      isMyMessage && "bg-amber-50",
                    )}
                  >
                    {/* 프로필 아이콘 */}
                    <div
                      className={cn(
                        "flex size-8 items-center justify-center rounded-full",
                        isMyMessage ? "bg-amber-500" : "bg-gray-300",
                      )}
                    >
                      <User size={16} className={isMyMessage ? "text-white" : "text-gray-600"} />
                    </div>

                    {/* 메시지 내용 */}
                    <div className="min-w-0 flex-1">
                      <div className="flex items-baseline gap-2">
                        <span
                          className={cn(
                            "font-medium text-sm",
                            isMyMessage ? "text-amber-700" : "text-gray-700",
                          )}
                        >
                          {msg.nickname}
                        </span>
                        <span className="text-xs text-gray-500">
                          {new Date(msg.timestamp ?? Date.now()).toLocaleTimeString()}
                        </span>
                      </div>
                      <p className="break-words text-sm text-gray-800">{msg.message}</p>
                    </div>
                  </div>
                </div>
              );
            })}
            <div ref={messagesEndRef} />
          </div>
        )}
      </div>

      {/* 메시지 입력 */}
      <form onSubmit={handleSendMessage} className="border-t p-3">
        <div className="flex gap-2">
          <input
            type="text"
            value={inputMessage}
            onChange={(e) => setInputMessage(e.target.value)}
            placeholder={authenticated ? "메시지를 입력하세요..." : "연결 중..."}
            disabled={!authenticated}
            className={cn(
              "flex-1 rounded-full border px-4 py-2 text-sm",
              "focus:border-amber-500 focus:outline-none",
              !authenticated && "bg-gray-100",
            )}
          />
          <button
            type="submit"
            disabled={!authenticated || !inputMessage.trim()}
            className={cn(
              "rounded-full p-2",
              "bg-amber-500 text-white",
              "transition-colors",
              "hover:bg-amber-600",
              "disabled:bg-gray-300 disabled:cursor-not-allowed",
            )}
          >
            <Send size={20} />
          </button>
        </div>
      </form>
    </div>
  );
}
