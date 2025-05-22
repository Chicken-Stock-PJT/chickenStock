package com.example.chickenstock.api

import android.content.Context
import com.example.chickenstock.data.TokenManager
import com.example.chickenstock.viewmodel.AuthViewModel
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
    private var instance: Retrofit? = null
    private var authViewModel: AuthViewModel? = null
    private const val BASE_URL = "https://chickenstock.shop/api/"
    private lateinit var appContext: Context

    fun setAuthViewModel(viewModel: AuthViewModel) {
        authViewModel = viewModel
    }

    fun getInstance(context: Context, ignoreAuthCheck: Boolean = false): Retrofit {
        appContext = context.applicationContext
        if (instance == null) {
            Log.d("RetrofitClient", "Retrofit 인스턴스 초기화 중...")
            
            // OkHttpClient 설정
            val client = OkHttpClient.Builder()
                .connectTimeout(30, TimeUnit.SECONDS)
                .readTimeout(30, TimeUnit.SECONDS)
                .writeTimeout(30, TimeUnit.SECONDS)
                
            // 토큰 확인 인터셉터 추가 (ignoreAuthCheck가 false일 때만)
            if (!ignoreAuthCheck) {
                client.addInterceptor { chain ->
                    val request = chain.request()
                    
                    // 토큰이 필요 없는 요청 확인 (로그인, 회원가입 등)
                    val skipAuth = request.url.toString().contains("/auth/login") ||
                            request.url.toString().contains("/auth/signup") ||
                            request.url.toString().contains("/auth/token/refresh")
                    
                    if (skipAuth) {
                        return@addInterceptor chain.proceed(request)
                    }
                    
                    val tokenManager = TokenManager.getInstance(appContext)
                    val accessToken = tokenManager.getAccessToken()
                    
                    // 토큰 확인
                    if (accessToken != null) {
                        // 토큰 만료 확인
                        if (tokenManager.isAccessTokenExpired()) {
                            if (authViewModel != null) {
                                Log.d("RetrofitClient", "토큰이 만료되었습니다. 새로운 토큰 요청 필요")
                                // 토큰 갱신 처리는 TokenAuthenticator에서 수행
                            } else {
                                Log.e("RetrofitClient", "AuthViewModel이 설정되지 않았습니다.")
                                throw IllegalStateException("AuthViewModel이 설정되지 않았습니다.")
                            }
                        }
                        
                        // 토큰 추가
                        val newRequest = request.newBuilder()
                            .header("Authorization", "Bearer $accessToken")
                            .build()
                        Log.d("RetrofitClient", "Authorization 헤더 추가됨: Bearer $accessToken")
                        return@addInterceptor chain.proceed(newRequest)
                    } else {
                        Log.d("RetrofitClient", "Authorization 헤더 없음")
                        return@addInterceptor chain.proceed(request)
                    }
                }
            }
            
            // 로깅 인터셉터 추가
            val loggingInterceptor = HttpLoggingInterceptor().apply {
                level = HttpLoggingInterceptor.Level.NONE
            }
            client.addInterceptor(loggingInterceptor)
            
            // 토큰 인증 실패 처리
            client.authenticator(TokenAuthenticator(context))
            
            // Retrofit 인스턴스 생성
            instance = Retrofit.Builder()
                .baseUrl(BASE_URL)
                .client(client.build())
                .addConverterFactory(GsonConverterFactory.create())
                .build()
            
            Log.d("RetrofitInit", "Retrofit 인스턴스 초기화 완료")
        }
        return instance!!
    }

    fun <T> createService(serviceClass: Class<T>, context: Context): T {
        return getInstance(context).create(serviceClass)
    }

    fun resetInstance() {
        instance = null
    }
} 