import { useState } from "react";
import { Send } from "lucide-react";
import { useChatNotificationStore } from "../model/store";
import { useWebSocket } from "../hooks/useWebSocket";
import { cn } from "@/shared/libs/utils";

export default function ChatTab() {
  const [inputMessage, setInputMessage] = useState("");
  const { messages, currentUser, authenticated, connected } = useChatNotificationStore();
  const { sendChatMessage } = useWebSocket();

  const handleSendMessage = (e: React.FormEvent) => {
    e.preventDefault();
    if (inputMessage.trim() && authenticated) {
      sendChatMessage(inputMessage);
      setInputMessage("");
    }
  };

  return (
    <div className="flex h-full flex-col">
      {/* 연결 상태 표시 */}
      <div className="px-4 py-2 text-center text-sm">
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
      <div className="flex-1 overflow-y-auto p-4">
        {messages.length === 0 ? (
          <div className="text-center text-gray-500">채팅을 시작해보세요!</div>
        ) : (
          messages.map((msg, index) => (
            <div
              key={index}
              className={cn(
                "mb-3 rounded-lg p-3 max-w-[80%]",
                msg.memberId === currentUser?.memberId
                  ? "ml-auto bg-amber-100 text-right"
                  : "mr-auto bg-gray-100",
              )}
            >
              <div className="text-xs font-medium text-gray-600">{msg.nickname}</div>
              <div className="mt-1 text-sm">{msg.message}</div>
              <div className="mt-1 text-xs text-gray-500">
                {new Date(msg.timestamp ?? Date.now()).toLocaleTimeString()}
              </div>
            </div>
          ))
        )}
      </div>

      {/* 메시지 입력 폼 */}
      <form onSubmit={handleSendMessage} className="border-t p-4">
        <div className="flex gap-2">
          <input
            type="text"
            value={inputMessage}
            onChange={(e) => setInputMessage(e.target.value)}
            placeholder={authenticated ? "메시지를 입력하세요..." : "연결 중..."}
            disabled={!authenticated}
            className={cn(
              "flex-1 rounded-lg border px-3 py-2 text-sm",
              "focus:border-amber-500 focus:outline-none",
              !authenticated && "bg-gray-100",
            )}
          />
          <button
            type="submit"
            disabled={!authenticated || !inputMessage.trim()}
            className={cn(
              "rounded-lg px-4 py-2",
              "bg-amber-500 text-white",
              "transition-colors",
              "hover:bg-amber-600",
              "disabled:bg-gray-300 disabled:cursor-not-allowed",
            )}
          >
            <Send size={18} />
          </button>
        </div>
      </form>
    </div>
  );
}
