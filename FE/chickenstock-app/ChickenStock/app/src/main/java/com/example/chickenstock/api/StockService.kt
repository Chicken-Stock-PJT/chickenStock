package com.example.chickenstock.api

import com.example.chickenstock.data.Stock
import retrofit2.Response
import retrofit2.http.GET

interface StockService {
    @GET("stocks/all")
    suspend fun getAllStocks(): Response<List<Stock>>
} 