package com.example.chickenstock.api

import retrofit2.Response
import retrofit2.http.GET
import retrofit2.http.Header
import com.example.chickenstock.model.PortfolioData

data class SimpleProfileResponse(
    val nickname: String,
    val memberMoney: String,
    val returnRate: String,
    val isOauth: String
)

interface MemberService {
    @GET("members/simple-profile")
    suspend fun getSimpleProfile(): Response<SimpleProfileResponse>

    @GET("members/portfolio")
    suspend fun getPortfolio(): Response<PortfolioData>
} 