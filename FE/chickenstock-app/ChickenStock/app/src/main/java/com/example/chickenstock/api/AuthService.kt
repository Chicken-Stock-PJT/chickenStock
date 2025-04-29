package com.example.chickenstock.api

import retrofit2.Response
import retrofit2.http.Body
import retrofit2.http.POST
import retrofit2.http.GET
import retrofit2.http.Query

data class EmailCheckRequest(
    val email: String
)

data class EmailCheckResponse(
    val success: Boolean,
    val message: String
)

data class VerifyCodeRequest(
    val email: String,
    val code: String
)

data class VerifyCodeResponse(
    val success: Boolean,
    val message: String
)

data class NicknameCheckRequest(
    val nickname: String
)

data class NicknameCheckResponse(
    val success: Boolean,
    val message: String,
    val duplicate: Boolean
)

data class LoginRequest(
    val email: String,
    val password: String,
    val platform: String = "mobile"  // 기본값으로 mobile 설정
)

data class LoginResponse(
    val accessToken: String,
    val refreshToken: String,
    val accessTokenExpiresIn: Long
)

data class TokenRefreshRequest(
    val accessToken: String,
    val refreshToken: String
)

data class TokenResponse(
    val accessToken: String,
    val refreshToken: String
)

data class SignupRequest(
    val email: String,
    val password: String,
    val nickname: String,
    val name: String
)

data class ChangePasswordRequest(
    val currentPassword: String,
    val newPassword: String,
    val checkPassword: String
)

interface AuthService {
    @POST("auth/check-email")
    suspend fun checkEmail(@Body request: EmailCheckRequest): Response<EmailCheckResponse>
    
    @POST("auth/send-code")
    suspend fun sendVerificationCode(@Body request: EmailCheckRequest): Response<Unit>
    
    @POST("auth/verify-code")
    suspend fun verifyCode(@Body request: VerifyCodeRequest): Response<VerifyCodeResponse>

    @POST("auth/check-nickname")
    suspend fun checkNickname(@Body request: NicknameCheckRequest): Response<NicknameCheckResponse>

    @POST("auth/reset-password-by-code")
    suspend fun resetPasswordByCode(@Body request: EmailCheckRequest): Response<Unit>

    @POST("auth/login")
    suspend fun login(@Body request: LoginRequest): Response<LoginResponse>

    @POST("auth/token/refresh-mobile")
    suspend fun refreshAccessToken(@Body request: TokenRefreshRequest): Response<TokenResponse>

    @POST("auth/token/refresh-all-mobile")
    suspend fun refreshAllTokens(@Body request: TokenRefreshRequest): Response<TokenResponse>

    @POST("auth/logout")
    suspend fun logout(): Response<Unit>

    @POST("auth/signup")
    suspend fun signup(@Body request: SignupRequest): Response<Unit>

    @POST("members/change-password")
    suspend fun changePassword(@Body request: ChangePasswordRequest): Response<Unit>
} 