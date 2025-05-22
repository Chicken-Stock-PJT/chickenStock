package com.example.chickenstock

import android.app.NotificationChannel
import android.app.NotificationManager
import android.app.PendingIntent
import android.content.Context
import android.content.Intent
import android.os.Build
import android.util.Log
import androidx.core.app.NotificationCompat
import com.google.firebase.messaging.FirebaseMessagingService
import com.google.firebase.messaging.RemoteMessage
import com.example.chickenstock.R
import com.example.chickenstock.data.TokenManager
import com.example.chickenstock.api.NotificationService
import com.example.chickenstock.api.FcmTokenRequest
import com.example.chickenstock.api.RetrofitClient
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch

class MyFirebaseMessagingService : FirebaseMessagingService() {
    override fun onMessageReceived(remoteMessage: RemoteMessage) {
        super.onMessageReceived(remoteMessage)
        Log.d("FCM", "onMessageReceived: "+remoteMessage.data)
        Log.d("FCM", "Notification: "+remoteMessage.notification)
        Log.d("FCM", "From: "+remoteMessage.from)
        
        // 데이터 메시지 또는 알림 메시지 모두 처리
        val title = remoteMessage.notification?.title ?: remoteMessage.data["title"] ?: "ChickenStock"
        val body = remoteMessage.notification?.body ?: remoteMessage.data["body"] ?: "새로운 알림이 있습니다."
        
        // 데이터 메시지의 추가 정보 로깅
        remoteMessage.data.forEach { (key, value) ->
            Log.d("FCM", "Data message - $key: $value")
        }
        
        sendNotification(title, body, remoteMessage.data)
    }

    override fun onNewToken(token: String) {
        super.onNewToken(token)
        Log.d("FCM", "onNewToken: $token")
        // 토큰이 갱신되면 서버에 등록
        val accessToken = TokenManager.getInstance(applicationContext).getAccessToken()
        if (!accessToken.isNullOrBlank()) {
            CoroutineScope(Dispatchers.IO).launch {
                try {
                    val service = RetrofitClient.getInstance(applicationContext).create(NotificationService::class.java)
                    val response = service.registerFcmToken("Bearer $accessToken", FcmTokenRequest(token))
                    Log.d("FCM", "onNewToken 서버 등록 응답: ${response.code()} ${response.body()?.message}")
                } catch (e: Exception) {
                    Log.e("FCM", "onNewToken 서버 등록 실패: ${e.message}")
                }
            }
        }
    }

    private fun sendNotification(title: String, messageBody: String, data: Map<String, String>) {
        val channelId = "chickenstock_channel"
        val notificationManager = getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager

        // 알림 클릭 시 앱 실행 및 데이터 전달
        val intent = Intent(this, MainActivity::class.java).apply {
            addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP)
            // 데이터 메시지의 정보를 인텐트에 추가
            data.forEach { (key, value) ->
                putExtra(key, value)
            }
        }
        val pendingIntent = PendingIntent.getActivity(this, 0, intent, PendingIntent.FLAG_ONE_SHOT or PendingIntent.FLAG_IMMUTABLE)

        val notificationBuilder = NotificationCompat.Builder(this, channelId)
            .setSmallIcon(R.drawable.ic_notification)
            .setContentTitle(title)
            .setContentText(messageBody)
            .setAutoCancel(true)
            .setContentIntent(pendingIntent)
            .setPriority(NotificationCompat.PRIORITY_HIGH)
            .setDefaults(NotificationCompat.DEFAULT_ALL) // 소리, 진동, LED 등 기본 알림 효과 사용

        // Android 8.0 이상은 채널 필요
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            val channel = NotificationChannel(
                channelId,
                "ChickenStock 알림",
                NotificationManager.IMPORTANCE_HIGH
            ).apply {
                description = "주식 거래 알림을 위한 채널입니다."
                enableLights(true)
                enableVibration(true)
                vibrationPattern = longArrayOf(0, 500, 200, 500)
            }
            notificationManager.createNotificationChannel(channel)
        }

        notificationManager.notify(System.currentTimeMillis().toInt(), notificationBuilder.build())
    }
} 