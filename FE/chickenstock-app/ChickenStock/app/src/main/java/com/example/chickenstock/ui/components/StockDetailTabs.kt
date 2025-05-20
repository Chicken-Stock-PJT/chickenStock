package com.example.chickenstock.ui.components

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material3.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.animation.core.tween
import androidx.compose.animation.core.animateDpAsState
import com.example.chickenstock.ui.theme.*

@Composable
fun StockDetailTabs(
    selectedTab: Int,
    onTabSelected: (Int) -> Unit,
    modifier: Modifier = Modifier
) {
    val tabs = listOf("차트", "상세 정보")
    
    BoxWithConstraints(
        modifier = modifier
            .fillMaxWidth()
            .height(48.dp)
    ) {
        val itemWidth = maxWidth / 2
        val centerOffset = (itemWidth - 120.dp) / 2

        val indicatorOffset by animateDpAsState(
            targetValue = itemWidth * selectedTab + centerOffset,
            animationSpec = tween(durationMillis = 300),
            label = "TabIndicatorAnimation"
        )

        // 인디케이터
        Box(
            modifier = Modifier
                .fillMaxWidth()
                .height(1.dp)
                .background(Gray200)
                .align(Alignment.BottomCenter)
        ) {
            Box(
                modifier = Modifier
                    .offset(x = indicatorOffset)
                    .width(120.dp)
                    .height(4.dp)
                    .background(Gray700)
            )
        }

        // 탭 버튼들
        Row(
            modifier = Modifier.fillMaxSize(),
            horizontalArrangement = Arrangement.Start,
            verticalAlignment = Alignment.CenterVertically
        ) {
            tabs.forEachIndexed { index, title ->
                Box(
                    modifier = Modifier
                        .width(itemWidth)
                        .fillMaxHeight()
                        .clickable { onTabSelected(index) },
                    contentAlignment = Alignment.Center
                ) {
                    Text(
                        text = title,
                        fontSize = 16.sp,
                        fontWeight = androidx.compose.ui.text.font.FontWeight.Bold,
                        color = if (selectedTab == index) Gray700 else Gray500,
                        fontFamily = SCDreamFontFamily
                    )
                }
            }
        }
    }
} 