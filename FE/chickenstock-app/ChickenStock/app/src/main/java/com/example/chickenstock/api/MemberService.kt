package com.example.chickenstock.api

import retrofit2.Response
import retrofit2.http.GET

data class SimpleProfileResponse(
    val nickname: String,
    val memberMoney: String,
    val returnRate: String,
    val isOauth: String
)

interface MemberService {
    @GET("members/simple-profile")
    suspend fun getSimpleProfile(): Response<SimpleProfileResponse>
} 