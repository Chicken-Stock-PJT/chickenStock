package com.example.chickenstock.api

import android.content.Context
import com.example.chickenstock.data.TokenManager
import okhttp3.OkHttpClient
import okhttp3.logging.HttpLoggingInterceptor
import retrofit2.Retrofit
import retrofit2.converter.gson.GsonConverterFactory
import java.util.concurrent.TimeUnit
import android.util.Log
import okhttp3.ResponseBody
import okio.Buffer
import okio.BufferedSource
import java.nio.charset.StandardCharsets

// ResponseBody 확장 함수 추가
private fun String.toResponseBody(contentType: okhttp3.MediaType?): ResponseBody {
    return ResponseBody.create(contentType, this)
}

object RetrofitClient {
    private const val BASE_URL = "https://k12a106.p.ssafy.io/api/"
    private var instance: Retrofit? = null

    fun resetInstance() {
        Log.d("RetrofitClient", "Retrofit 인스턴스 초기화 중...")
        instance = null
        Log.d("RetrofitClient", "Retrofit 인스턴스 초기화 완료")
    }

    fun getInstance(context: Context): Retrofit {
        return instance ?: synchronized(this) {
            Log.d("RetrofitClient", "Retrofit 인스턴스 생성 중...")
            val tokenManager = TokenManager.getInstance(context)
            
            val loggingInterceptor = HttpLoggingInterceptor().apply {
                level = HttpLoggingInterceptor.Level.HEADERS
                setLevel(HttpLoggingInterceptor.Level.HEADERS)
            }

            val client = OkHttpClient.Builder()
                .addInterceptor { chain ->
                    val token = tokenManager.getAccessToken()
                    val original = chain.request()
                    val builder = original.newBuilder()
                    if (!token.isNullOrBlank()) {
                        builder.header("Authorization", "Bearer $token")
                    }
                    val request = builder.build()
                    Log.d("RetrofitClient", "API Request: ${request.url}")
                    
                    val response = chain.proceed(request)
                    
                    // 응답 본문을 안전하게 읽기
                    val responseCode = response.code
                    val responseMessage = response.message
                    
                    Log.d("RetrofitClient", "API Response Code: $responseCode")
                    Log.d("RetrofitClient", "API Response Message: $responseMessage")
                    
                    // 원본 응답 그대로 반환 (본문 소비 없음)
                    response
                }
                .addInterceptor(loggingInterceptor)
                .authenticator(TokenAuthenticator(context))
                .connectTimeout(30, TimeUnit.SECONDS)
                .readTimeout(30, TimeUnit.SECONDS)
                .writeTimeout(30, TimeUnit.SECONDS)
                // 리다이렉트 최대 횟수 제한
                .followRedirects(true)
                .followSslRedirects(true)
                .retryOnConnectionFailure(true)
                .build()

            Retrofit.Builder()
                .baseUrl(BASE_URL)
                .client(client)
                .addConverterFactory(GsonConverterFactory.create())
                .build().also { instance = it }
        }
    }

    fun <T> createService(serviceClass: Class<T>, context: Context): T {
        return getInstance(context).create(serviceClass)
    }
} 