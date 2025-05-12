package com.example.chickenstock.ui.components

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import com.example.chickenstock.api.StockBidAsk
import java.text.NumberFormat
import java.util.*

@Composable
fun StockBidAskView(
    stockBidAsk: StockBidAsk,
    modifier: Modifier = Modifier
) {
    Column(
        modifier = modifier
            .fillMaxWidth()
            .padding(8.dp)
    ) {
        // 헤더
        Row(
            modifier = Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.SpaceBetween
        ) {
            Text(
                text = "매도잔량",
                modifier = Modifier.weight(1f),
                textAlign = TextAlign.Center,
                fontSize = 12.sp,
                color = Color(0xFFFF4444)
            )
            Text(
                text = "호가",
                modifier = Modifier.weight(1f),
                textAlign = TextAlign.Center,
                fontSize = 12.sp
            )
            Text(
                text = "매수잔량",
                modifier = Modifier.weight(1f),
                textAlign = TextAlign.Center,
                fontSize = 12.sp,
                color = Color(0xFF4444FF)
            )
        }

        // 매도 호가 (8~1)
        for (i in 8 downTo 1) {
            val price = stockBidAsk.askPrices["$i"]?.replace("""[+\-]""".toRegex(), "") ?: "0"
            val volume = stockBidAsk.askVolumes["$i"] ?: "0"
            
            Row(
                modifier = Modifier
                    .fillMaxWidth()
                    .background(Color(0xFFFFEEEE)),
                horizontalArrangement = Arrangement.SpaceBetween,
                verticalAlignment = Alignment.CenterVertically
            ) {
                Text(
                    text = NumberFormat.getNumberInstance(Locale.KOREA).format(volume.toLongOrNull() ?: 0),
                    modifier = Modifier.weight(1f),
                    textAlign = TextAlign.End,
                    fontSize = 12.sp,
                    color = Color(0xFFFF4444)
                )
                Text(
                    text = NumberFormat.getNumberInstance(Locale.KOREA).format(price.toLongOrNull() ?: 0),
                    modifier = Modifier.weight(1f),
                    textAlign = TextAlign.Center,
                    fontSize = 12.sp,
                    color = Color(0xFFFF4444)
                )
                Text(
                    text = "",
                    modifier = Modifier.weight(1f),
                    textAlign = TextAlign.Start,
                    fontSize = 12.sp
                )
            }
        }

        // 매수 호가 (1~8)
        for (i in 1..8) {
            val price = stockBidAsk.bidPrices["$i"]?.replace("""[+\-]""".toRegex(), "") ?: "0"
            val volume = stockBidAsk.bidVolumes["$i"] ?: "0"
            
            Row(
                modifier = Modifier
                    .fillMaxWidth()
                    .background(Color(0xFFEEEEFF)),
                horizontalArrangement = Arrangement.SpaceBetween,
                verticalAlignment = Alignment.CenterVertically
            ) {
                Text(
                    text = "",
                    modifier = Modifier.weight(1f),
                    textAlign = TextAlign.End,
                    fontSize = 12.sp
                )
                Text(
                    text = NumberFormat.getNumberInstance(Locale.KOREA).format(price.toLongOrNull() ?: 0),
                    modifier = Modifier.weight(1f),
                    textAlign = TextAlign.Center,
                    fontSize = 12.sp,
                    color = Color(0xFF4444FF)
                )
                Text(
                    text = NumberFormat.getNumberInstance(Locale.KOREA).format(volume.toLongOrNull() ?: 0),
                    modifier = Modifier.weight(1f),
                    textAlign = TextAlign.Start,
                    fontSize = 12.sp,
                    color = Color(0xFF4444FF)
                )
            }
        }
    }
} 