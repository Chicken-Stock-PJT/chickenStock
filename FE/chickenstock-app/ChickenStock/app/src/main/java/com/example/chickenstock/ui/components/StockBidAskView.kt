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
    // 매도/매수 잔량 리스트 추출 (8~1, 1~8)
    val askVolumes = (8 downTo 1).map { stockBidAsk.askVolumes["$it"]?.replace(",", "")?.toIntOrNull() ?: 0 }
    val bidVolumes = (1..8).map { stockBidAsk.bidVolumes["$it"]?.replace(",", "")?.toIntOrNull() ?: 0 }
    val maxAskVolume = askVolumes.maxOrNull()?.takeIf { it > 0 } ?: 1
    val maxBidVolume = bidVolumes.maxOrNull()?.takeIf { it > 0 } ?: 1

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
                text = "매수호가잔량",
                modifier = Modifier.weight(1f),
                textAlign = TextAlign.End,
                fontSize = 12.sp,
                color = Color(0xFFD32F2F)
            )
            Text(
                text = "호가",
                modifier = Modifier.weight(1f),
                textAlign = TextAlign.Center,
                fontSize = 12.sp,
                color = Color.Black
            )
            Text(
                text = "매도호가잔량",
                modifier = Modifier.weight(1f),
                textAlign = TextAlign.Start,
                fontSize = 12.sp,
                color = Color(0xFF1976D2)
            )
        }
        // 헤더와 데이터 사이 구분선
        androidx.compose.material3.Divider(
            modifier = Modifier
                .fillMaxWidth()
                .padding(vertical = 4.dp),
            color = Color(0xFFEEEEEE),
            thickness = 1.dp
        )

        // 매도 호가 (8~1)
        for (i in 8 downTo 1) {
            val price = stockBidAsk.askPrices["$i"]?.replace("[+\\-]".toRegex(), "") ?: "0"
            val volume = stockBidAsk.askVolumes["$i"]?.replace(",", "")?.toIntOrNull() ?: 0
            val ratio = volume.toFloat() / maxAskVolume
            Row(
                modifier = Modifier
                    .fillMaxWidth()
                    .height(28.dp),
                verticalAlignment = Alignment.CenterVertically
            ) {
                Spacer(modifier = Modifier.weight(1f))
                Text(
                    text = NumberFormat.getNumberInstance(Locale.KOREA).format(price.toLongOrNull() ?: 0),
                    modifier = Modifier.weight(1f),
                    textAlign = TextAlign.Center,
                    fontSize = 13.sp,
                    color = Color(0xFF1976D2)
                )
                Box(
                    modifier = Modifier
                        .weight(1f)
                        .fillMaxHeight()
                        .padding(start = 4.dp)
                        .background(Color.Transparent)
                ) {
                    Box(
                        modifier = Modifier
                            .fillMaxHeight()
                            .fillMaxWidth(ratio)
                            .align(Alignment.CenterStart)
                            .background(Color(0xFFBBDEFB), shape = androidx.compose.foundation.shape.RoundedCornerShape(4.dp))
                    )
                    Text(
                        text = NumberFormat.getNumberInstance(Locale.KOREA).format(volume),
                        color = Color.Black,
                        fontSize = 12.sp,
                        modifier = Modifier
                            .align(Alignment.CenterStart)
                            .padding(start = 8.dp)
                    )
                }
            }
        }

        // 매수 호가 (1~8)
        for (i in 1..8) {
            val price = stockBidAsk.bidPrices["$i"]?.replace("[+\\-]".toRegex(), "") ?: "0"
            val volume = stockBidAsk.bidVolumes["$i"]?.replace(",", "")?.toIntOrNull() ?: 0
            val ratio = volume.toFloat() / maxBidVolume
            Row(
                modifier = Modifier
                    .fillMaxWidth()
                    .height(28.dp),
                verticalAlignment = Alignment.CenterVertically
            ) {
                Box(
                    modifier = Modifier
                        .weight(1f)
                        .fillMaxHeight()
                        .padding(end = 4.dp)
                        .background(Color.Transparent)
                ) {
                    Box(
                        modifier = Modifier
                            .fillMaxHeight()
                            .fillMaxWidth(ratio)
                            .align(Alignment.CenterEnd)
                            .background(Color(0xFFFFCDD2), shape = androidx.compose.foundation.shape.RoundedCornerShape(4.dp))
                    )
                    Text(
                        text = NumberFormat.getNumberInstance(Locale.KOREA).format(volume),
                        color = Color.Black,
                        fontSize = 12.sp,
                        modifier = Modifier
                            .align(Alignment.CenterEnd)
                            .padding(end = 8.dp)
                    )
                }
                Text(
                    text = NumberFormat.getNumberInstance(Locale.KOREA).format(price.toLongOrNull() ?: 0),
                    modifier = Modifier.weight(1f),
                    textAlign = TextAlign.Center,
                    fontSize = 13.sp,
                    color = Color(0xFFD32F2F)
                )
                Spacer(modifier = Modifier.weight(1f))
            }
        }
    }
} 