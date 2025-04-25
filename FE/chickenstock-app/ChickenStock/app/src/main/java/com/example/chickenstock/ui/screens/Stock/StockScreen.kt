package com.example.chickenstock.ui.screens.stock

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.KeyboardArrowRight
import androidx.compose.material.icons.outlined.FavoriteBorder
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.navigation.NavHostController
import com.example.chickenstock.ui.theme.SCDreamFontFamily
import com.example.chickenstock.ui.theme.Gray0
import com.example.chickenstock.ui.theme.Gray700
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.shape.CircleShape
import coil.compose.AsyncImage
import androidx.compose.ui.draw.clip
import com.example.chickenstock.ui.components.SegmentedControl
import com.example.chickenstock.ui.components.StockListItem
import com.example.chickenstock.ui.components.StockItem

data class StockItem(
    val stockCode: String,
    val stockName: String,
    val market: String,
    val currentPrice: String,
    val fluctuationRate: String,
    val tradeAmount: String
)

val dummyStocks = listOf(
    StockItem("086520", "에코프로", "KOSDAQ", "1230000", "+5.60", "1200"),
    StockItem("005930", "삼성전자", "KOSPI", "73200", "-0.40", "950"),
    StockItem("247540", "에코프로비엠", "KOSDAQ", "438000", "+3.10", "640"),
    StockItem("035720", "카카오", "KOSPI", "48500", "-1.20", "850"),
    StockItem("035420", "NAVER", "KOSPI", "198000", "+2.30", "780")
)

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun StockScreen(
    navController: NavHostController
) {
    var selectedMarketIndex by remember { mutableStateOf(0) }
    var selectedSortIndex by remember { mutableStateOf(0) }
    val sortOptions = listOf("거래대금", "급상승", "급하락", "거래량")

    Scaffold(
        containerColor = Gray0,
    ) { paddingValues ->
        Column(
            modifier = Modifier
                .fillMaxSize()
                .padding(paddingValues)
        ) {
            // 첫 번째 SegmentedControl (시장 선택)
            SegmentedControl(
                items = listOf("전체", "코스피", "코스닥"),
                selectedIndex = selectedMarketIndex,
                onSelectedIndexChange = { selectedMarketIndex = it },
                modifier = Modifier.padding(horizontal = 32.dp)
            )

            // 두 번째 SegmentedControl (정렬 기준)
            SegmentedControl(
                items = sortOptions,
                selectedIndex = selectedSortIndex,
                onSelectedIndexChange = { selectedSortIndex = it },
                modifier = Modifier.padding(horizontal = 32.dp, vertical = 8.dp)
            )
            
            LazyColumn(
                modifier = Modifier
                    .fillMaxSize()
                    .padding(horizontal = 32.dp)
            ) {
                items(dummyStocks) { stock ->
                    StockListItem(
                        stock = stock,
                        navController = navController,
                        onFavoriteClick = { /* 즐겨찾기 기능 */ }
                    )
                }
            }
        }
    }
}

@Composable
fun SegmentedControl(
    items: List<String>,
    selectedIndex: Int,
    onSelectedIndexChange: (Int) -> Unit,
    modifier: Modifier = Modifier
) {
    Surface(
        modifier = modifier.fillMaxWidth(),
        shape = RoundedCornerShape(12.dp),
        color = Color(0xFFF5F5F5)
    ) {
        Row(
            modifier = Modifier.padding(4.dp)
        ) {
            items.forEachIndexed { index, item ->
                Box(
                    modifier = Modifier
                        .weight(1f)
                        .height(40.dp)
                        .background(
                            color = if (selectedIndex == index) Color.White else Color.Transparent,
                            shape = RoundedCornerShape(8.dp)
                        )
                        .clickable { onSelectedIndexChange(index) },
                    contentAlignment = Alignment.Center
                ) {
                    Text(
                        text = item,
                        color = if (selectedIndex == index) Color.Black else Color.Gray,
                        fontFamily = SCDreamFontFamily,
                        fontWeight = if (selectedIndex == index) FontWeight.Bold else FontWeight.Normal
                    )
                }
            }
        }
    }
}

@Composable
fun StockListItem(stock: StockItem, navController: NavHostController, onFavoriteClick: () -> Unit) {
    val isUp = stock.fluctuationRate.startsWith("+")
    val fluctuationColor = when {
        stock.fluctuationRate.startsWith("+") -> Color.Red
        stock.fluctuationRate.startsWith("-") -> Color.Blue
        else -> Color.Gray
    }

    Surface(
        modifier = Modifier
            .fillMaxWidth()
            .padding(vertical = 8.dp),
        shape = RoundedCornerShape(16.dp),
        color = Color(0xFFF8F8F8),
        shadowElevation = 2.dp
    ) {
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .padding(16.dp),
            horizontalArrangement = Arrangement.SpaceBetween,
            verticalAlignment = Alignment.CenterVertically
        ) {
            // 왼쪽 정렬: 하트, 숫자, 이름&금액
            Row(
                verticalAlignment = Alignment.CenterVertically,
                modifier = Modifier.weight(1f)
            ) {
                // 하트 아이콘
                IconButton(
                    onClick = { onFavoriteClick() },
                    modifier = Modifier.size(24.dp)
                ) {
                    Icon(
                        imageVector = Icons.Outlined.FavoriteBorder,
                        contentDescription = "Favorite",
                        tint = Color.LightGray,
                        modifier = Modifier.size(20.dp)
                    )
                }

                Spacer(modifier = Modifier.width(8.dp))

                // 종목 로고
                AsyncImage(
                    model = "https://thumb.tossinvest.com/image/resized/96x0/https%3A%2F%2Fstatic.toss.im%2Fpng-icons%2Fsecurities%2Ficn-sec-fill-${stock.stockCode}.png",
                    contentDescription = "주식 로고",
                    modifier = Modifier
                        .size(40.dp) // ← 🔼 기존보다 크게
                        .clip(CircleShape) // ← 완전한 원
                        .background(Color.White) // 배경 유지
                )
                
                // 회사 이름과 가격
                Column(
                    modifier = Modifier.padding(start = 8.dp)
                ) {
                    Text(
                        text = stock.stockName,
                        fontSize = 18.sp,
                        fontWeight = FontWeight.W700,
                        fontFamily = SCDreamFontFamily
                    )
                    Row(verticalAlignment = Alignment.CenterVertically) {
                        Text(
                            text = "${stock.currentPrice}원",
                            fontSize = 16.sp,
                            fontWeight = FontWeight.W500,
                            fontFamily = SCDreamFontFamily
                        )
                        Text(
                            text = " ${stock.fluctuationRate}%",
                            fontSize = 16.sp,
                            fontWeight = FontWeight.W500,
                            color = fluctuationColor,
                            fontFamily = SCDreamFontFamily
                        )
                    }
                }
            }
            
            // 오른쪽 정렬: 거래대금과 화살표
            Row(
                verticalAlignment = Alignment.CenterVertically,
                horizontalArrangement = Arrangement.End
            ) {
                Text(
                    text = "${stock.tradeAmount}원",
                    fontSize = 16.sp,
                    fontWeight = FontWeight.W500,
                    fontFamily = SCDreamFontFamily,
                    color = Color.Gray,
                    modifier = Modifier.padding(end = 8.dp)
                )
                Icon(
                    imageVector = Icons.Filled.KeyboardArrowRight,
                    contentDescription = "더 보기",
                    tint = Color.Gray,
                    modifier = Modifier.size(20.dp)
                )
            }
        }
    }
} 