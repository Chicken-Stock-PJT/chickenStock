package com.example.chickenstock.api

import android.content.Context
import com.example.chickenstock.data.TokenManager
import okhttp3.OkHttpClient
import retrofit2.Retrofit
import retrofit2.converter.gson.GsonConverterFactory
import java.util.concurrent.TimeUnit

object RetrofitClient {
    private const val BASE_URL = "https://k12a106.p.ssafy.io/api/"
    internal var retrofit: Retrofit? = null

    fun getInstance(context: Context): Retrofit {
        if (retrofit == null) {
            val tokenManager = TokenManager.getInstance(context)
            
            val okHttpClient = OkHttpClient.Builder()
                .addInterceptor { chain ->
                    val original = chain.request()
                    val builder = original.newBuilder()
                    
                    // Access token이 있다면 헤더에 추가
                    tokenManager.getAccessToken()?.let { token ->
                        builder.addHeader("Authorization", "Bearer $token")
                    }
                    
                    val request = builder.build()
                    chain.proceed(request)
                }
                .authenticator(TokenAuthenticator(context))
                .connectTimeout(30, TimeUnit.SECONDS)
                .readTimeout(30, TimeUnit.SECONDS)
                .writeTimeout(30, TimeUnit.SECONDS)
                .build()

            retrofit = Retrofit.Builder()
                .baseUrl(BASE_URL)
                .client(okHttpClient)
                .addConverterFactory(GsonConverterFactory.create())
                .build()
        }
        return retrofit!!
    }
} 