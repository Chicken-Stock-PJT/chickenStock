package com.example.chickenstock.ui.components

import androidx.compose.foundation.layout.*
import androidx.compose.material3.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.sp
import com.example.chickenstock.api.StockPrice
import java.text.NumberFormat
import java.util.*

@Composable
fun AnimatedStockPrice(
    stockPrice: StockPrice,
    modifier: Modifier = Modifier
) {
    val priceColor = when {
        stockPrice.isPositiveChange() -> Color(0xFFFF4444)
        !stockPrice.isPositiveChange() -> Color(0xFF4444FF)
        else -> Color.Black
    }

    Column(modifier = modifier) {
        Text(
            text = NumberFormat.getNumberInstance(Locale.KOREA)
                .format(stockPrice.currentPrice.replace(",", "").toLongOrNull() ?: 0),
            color = priceColor,
            fontSize = 16.sp,
            fontWeight = FontWeight.Bold
        )
        Text(
            text = stockPrice.changeRate,
            color = priceColor,
            fontSize = 14.sp
        )
    }
} 