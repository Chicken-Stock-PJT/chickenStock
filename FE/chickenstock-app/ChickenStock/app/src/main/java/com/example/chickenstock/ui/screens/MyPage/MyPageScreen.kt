package com.example.chickenstock.ui.screens.mypage

import androidx.compose.foundation.Canvas
import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Person
import androidx.compose.material.icons.filled.Settings
import androidx.compose.material.icons.filled.KeyboardArrowRight
import androidx.compose.material.icons.outlined.FavoriteBorder
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.StrokeCap
import androidx.compose.ui.graphics.drawscope.Stroke
import androidx.compose.ui.graphics.drawscope.DrawScope
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.navigation.NavController
import com.example.chickenstock.R
import com.example.chickenstock.ui.theme.*
import androidx.compose.animation.core.*
import kotlinx.coroutines.launch
import coil.compose.AsyncImage

data class Portfolio(
    val stockName: String,
    val stockCount: Int,
    val stockLogo: Int,
    val purchasePrice: Int,
    val currentPrice: Int,
    val ratio: Float,
    val investment: Int,
    val profit: Int,
    val profitRatio: Float
)

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun MyPageScreen(navController: NavController) {
    // 탭 상태 관리
    var selectedTabIndex by remember { mutableStateOf(0) }
    val tabs = listOf("포트폴리오", "거래 기록", "관심 종목")
    
    // 샘플 포트폴리오 데이터
    val portfolios = listOf(
        Portfolio(
            stockName = "삼성전자",
            stockCount = 36,
            stockLogo = R.drawable.logo, // 임시 로고
            purchasePrice = 55000,
            currentPrice = 77500,
            ratio = 0.667f,
            investment = 2000000,
            profit = 1000000,
            profitRatio = 1.0f
        ),
        Portfolio(
            stockName = "한화오션",
            stockCount = 8,
            stockLogo = R.drawable.logo, // 임시 로고
            purchasePrice = 80000,
            currentPrice = 120000,
            ratio = 0.333f,
            investment = 1000000,
            profit = -500000,
            profitRatio = -0.333f
        )
    )
    
    // 자산 정보
    val totalAsset = 3001000
    val availableAsset = 1000
    val investedAsset = 3000000
    val totalProfit = 0.03f
    
    // 샘플 거래 기록 데이터
    val tradeRecords = listOf(
        TradeRecord(
            stockName = "삼성전자",
            stockLogo = R.drawable.logo,
            tradeType = TradeType.SELL,
            price = 50000,
            quantity = 2,
            date = "2025.04.17",
            time = "12:22:05"
        ),
        TradeRecord(
            stockName = "삼성전자",
            stockLogo = R.drawable.logo,
            tradeType = TradeType.BUY,
            price = 51000,
            quantity = 1,
            date = "2025.04.16",
            time = "11:08:27" 
        ),
        TradeRecord(
            stockName = "삼성전자",
            stockLogo = R.drawable.logo,
            tradeType = TradeType.BUY,
            price = 53000,
            quantity = 3,
            date = "2025.04.13",
            time = "14:11:59"
        )
    )
    
    // 샘플 관심 종목 데이터
    val favoriteStocks = listOf(
        FavoriteStock(
            stockName = "삼성전자",
            stockLogo = R.drawable.logo,
            currentPrice = 54900,
            fluctuationRate = -3.1f
        ),
        FavoriteStock(
            stockName = "SK하이닉스",
            stockLogo = R.drawable.logo,
            currentPrice = 174100,
            fluctuationRate = -3.5f
        ),
        FavoriteStock(
            stockName = "한화오션",
            stockLogo = R.drawable.logo,
            currentPrice = 77100,
            fluctuationRate = -2.8f
        )
    )
    
    Scaffold(
        containerColor = Color.White
    ) { innerPadding ->
        Column(
            modifier = Modifier
                .fillMaxSize()
                .padding(innerPadding)
        ) {
            // 프로필 섹션 (고정)
            Box(
                modifier = Modifier
                    .fillMaxWidth()
                    .background(Color(0xFFFFFBEB))
                    .padding(16.dp)
            ) {
                // 설정 아이콘
                IconButton(
                    onClick = { /* 설정 화면으로 이동 */ },
                    modifier = Modifier.align(Alignment.TopEnd)
                ) {
                    Icon(
                        imageVector = Icons.Default.Settings,
                        contentDescription = "설정",
                        tint = Color.Black
                    )
                }
                
                // 프로필 정보
                Column(
                    horizontalAlignment = Alignment.CenterHorizontally,
                    modifier = Modifier.fillMaxWidth()
                ) {
                    // 프로필 이미지
                    Box(
                        modifier = Modifier
                            .size(80.dp)
                            .clip(CircleShape)
                            .background(Color.Black)
                            .padding(16.dp)
                    ) {
                        Icon(
                            imageVector = Icons.Default.Person,
                            contentDescription = "프로필",
                            tint = Color.White,
                            modifier = Modifier.size(48.dp)
                        )
                    }
                    
                    Spacer(modifier = Modifier.height(8.dp))
                    
                    // 사용자 이름
                    Text(
                        text = "김씨피",
                        fontSize = 18.sp,
                        fontWeight = FontWeight.Bold,
                        fontFamily = SCDreamFontFamily
                    )
                    
                    Spacer(modifier = Modifier.height(4.dp))
                    
                    // 이메일
                    Text(
                        text = "ssstykim@gmail.com",
                        fontSize = 14.sp,
                        color = Color.Gray,
                        fontFamily = SCDreamFontFamily
                    )
                }
            }
            
            // 탭 섹션 (고정)
            Row(
                modifier = Modifier.fillMaxWidth()
            ) {
                tabs.forEachIndexed { index, title ->
                    TabButton(
                        title = title,
                        isSelected = index == selectedTabIndex,
                        onClick = { selectedTabIndex = index },
                        modifier = Modifier.weight(1f)
                    )
                }
            }
            
            // 선택된 탭에 따라 내용 표시
            when (selectedTabIndex) {
                0 -> PortfolioTabContent(
                    portfolios = portfolios,
                    totalAsset = totalAsset,
                    availableAsset = availableAsset,
                    investedAsset = investedAsset,
                    totalProfit = totalProfit
                )
                1 -> TradeHistoryTabContent(tradeRecords = tradeRecords)
                2 -> FavoriteStocksTabContent(
                    favoriteStocks = favoriteStocks,
                    navController = navController
                )
            }
        }
    }
}

@Composable
fun TabButton(
    title: String,
    isSelected: Boolean,
    onClick: () -> Unit,
    modifier: Modifier = Modifier
) {
    Column(
        horizontalAlignment = Alignment.CenterHorizontally,
        verticalArrangement = Arrangement.Center,
        modifier = modifier
            .height(48.dp)
            .clickable { onClick() }
    ) {
        Box(
            modifier = Modifier
                .fillMaxWidth()
                .weight(1f),
            contentAlignment = Alignment.Center
        ) {
            Text(
                text = title,
                fontSize = 16.sp,
                fontWeight = if (isSelected) FontWeight.Bold else FontWeight.Normal,
                color = if (isSelected) Color.Black else Color.Gray,
                textAlign = TextAlign.Center,
                fontFamily = SCDreamFontFamily
            )
        }
        
        Box(
            modifier = Modifier
                .height(2.dp)
                .fillMaxWidth()
                .background(if (isSelected) Primary500 else Color.Transparent)
        )
    }
}

@Composable
fun AssetInfoRow(
    label: String,
    value: String,
    color: Color = Color.Black
) {
    Row(
        modifier = Modifier.fillMaxWidth(),
        horizontalArrangement = Arrangement.SpaceBetween
    ) {
        Text(
            text = label,
            fontSize = 14.sp,
            fontFamily = SCDreamFontFamily
        )
        Text(
            text = value,
            fontSize = 14.sp,
            color = color,
            fontWeight = FontWeight.Bold,
            fontFamily = SCDreamFontFamily
        )
    }
}

@Composable
fun ChartLegendItem(
    color: Color,
    label: String,
    value: String
) {
    Row(
        verticalAlignment = Alignment.CenterVertically
    ) {
        Box(
            modifier = Modifier
                .size(12.dp)
                .background(color)
        )
        Spacer(modifier = Modifier.width(4.dp))
        Text(
            text = label,
            fontSize = 14.sp,
            fontFamily = SCDreamFontFamily
        )
        Spacer(modifier = Modifier.width(4.dp))
        Text(
            text = value,
            fontSize = 14.sp,
            fontFamily = SCDreamFontFamily
        )
    }
}

@Composable
fun AccountBookGraph(
    modifier: Modifier = Modifier,
    colors: List<Color>,
    data: List<Float>,
    graphHeight: Int
) {
    val total = data.sum().takeIf { it > 0 } ?: 1f // 0으로 나누기 방지
    val angles = data.map { it / total * 360f }
    
    // 애니메이션을 위한 각도 값 설정
    val angleList = remember(data) { angles.map { Animatable(0f) } }
    
    // 데이터가 변경될 때마다 애니메이션 시작
    LaunchedEffect(data) {
        angleList.forEachIndexed { index, animatable ->
            launch {
                animatable.snapTo(0f) // 초기 각도를 0으로 설정
                animatable.animateTo(
                    targetValue = angles[index],
                    animationSpec = tween(
                        durationMillis = 1000, // 1초 동안 애니메이션
                        easing = LinearOutSlowInEasing // 천천히 끝나는 애니메이션 효과
                    )
                )
            }
        }
    }

    // Canvas를 사용하여 그래프를 그리고 `graphHeight.dp`를 픽셀 단위로 변환하여 그래프의 높이를 설정
    Canvas(modifier = modifier.height(graphHeight.dp)) {
        // 그래프의 선 두께를 지정
        val strokeWidth = graphHeight.dp.toPx() / 4
        // 원형 그래프의 반지름 설정
        val radius = (graphHeight.dp.toPx() - strokeWidth) / 2
        // 그래프의 중심 좌표
        val centerX = size.width / 2f
        val centerY = radius + strokeWidth / 2

        if (angleList.isNotEmpty()) {
            var startAngle = -90f // 12시 방향을 0도로 시작

            // 리스트를 순회하면서 각 데이터 항목에 대한 원호를 그린다
            angleList.forEachIndexed { index, animatable ->
                val color = if (index < colors.size) colors[index] else Color.Gray
                val sweepAngle = animatable.value // 애니메이션된 각도 값

                drawArc(
                    color = color, // 그래프 부분의 색상
                    startAngle = startAngle, // 원호의 시작 각도
                    sweepAngle = sweepAngle, // 원호의 중심각 (애니메이션됨)
                    useCenter = false, // 원호만 그림 (부채꼴 아님)
                    style = Stroke(width = strokeWidth), // 선 두께 설정
                    topLeft = Offset(centerX - radius, centerY - radius), // 왼쪽 상단 좌표
                    size = Size(radius * 2, radius * 2) // 원호를 그릴 사각형의 크기
                )

                // 다음 항목의 시작 각도 업데이트
                startAngle += sweepAngle
            }
        } else {
            // 데이터가 없는 경우 회색 원 표시
            drawCircle(
                color = Color.Gray.copy(alpha = 0.3f),
                radius = radius,
                center = Offset(centerX, centerY),
                style = Stroke(width = strokeWidth)
            )
        }
    }
}

@Composable
fun PortfolioItem(portfolio: Portfolio) {
    Card(
        modifier = Modifier
            .fillMaxWidth()
            .padding(horizontal = 16.dp, vertical = 8.dp),
        shape = RoundedCornerShape(12.dp),
        colors = CardDefaults.cardColors(
            containerColor = Color(0xFFF5F5F5)
        ),
        elevation = CardDefaults.cardElevation(
            defaultElevation = 0.dp
        )
    ) {
        Column(
            modifier = Modifier
                .fillMaxWidth()
                .padding(16.dp)
        ) {
            // 주식 기본 정보
            Row(
                verticalAlignment = Alignment.CenterVertically,
                horizontalArrangement = Arrangement.SpaceBetween,
                modifier = Modifier.fillMaxWidth()
            ) {
                // 로고와 주식명
                Row(
                    verticalAlignment = Alignment.CenterVertically
                ) {
                    // 로고
                    Box(
                        modifier = Modifier
                            .size(40.dp)
                            .clip(CircleShape)
                            .background(if (portfolio.stockName == "삼성전자") Color(0xFF1428A0) else Color(0xFFFF8A00))
                            .padding(8.dp),
                        contentAlignment = Alignment.Center
                    ) {
                        Text(
                            text = portfolio.stockName.first().toString(),
                            color = Color.White,
                            fontWeight = FontWeight.Bold
                        )
                    }
                    
                    Spacer(modifier = Modifier.width(12.dp))
                    
                    // 주식 이름
                    Text(
                        text = portfolio.stockName,
                        fontSize = 16.sp,
                        fontWeight = FontWeight.Bold,
                        fontFamily = SCDreamFontFamily
                    )
                }
                
                // 보유 수량
                Text(
                    text = "${portfolio.stockCount}주",
                    fontSize = 16.sp,
                    fontWeight = FontWeight.Bold,
                    fontFamily = SCDreamFontFamily
                )
            }
            
            Spacer(modifier = Modifier.height(12.dp))
            
            // 가격 정보
            Column(
                modifier = Modifier.fillMaxWidth()
            ) {
                Text(
                    text = "평균 매수 금액: ${portfolio.purchasePrice.toFormattedString()} 원",
                    fontSize = 14.sp,
                    color = Color.Gray,
                    fontFamily = SCDreamFontFamily
                )
                
                Spacer(modifier = Modifier.height(4.dp))
                
                Text(
                    text = "현재 주식 가치: ${portfolio.currentPrice.toFormattedString()} 원",
                    fontSize = 14.sp,
                    color = Color.Gray,
                    fontFamily = SCDreamFontFamily
                )
            }
            
            Spacer(modifier = Modifier.height(12.dp))
            
            // 자산 비중
            Text(
                text = "전체 자산 중 ${(portfolio.ratio * 100).toInt()}.${((portfolio.ratio * 1000) % 10).toInt()}%",
                fontSize = 14.sp,
                color = Color.Blue,
                fontFamily = SCDreamFontFamily,
                modifier = Modifier.align(Alignment.End)
            )
            
            Spacer(modifier = Modifier.height(12.dp))
            
            // 투자금액 및 수익 정보
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.SpaceBetween,
                verticalAlignment = Alignment.CenterVertically
            ) {
                Text(
                    text = "${portfolio.investment.toFormattedString()} 원",
                    fontSize = 18.sp,
                    fontWeight = FontWeight.Bold,
                    fontFamily = SCDreamFontFamily
                )
                Text(
                    text = "${if (portfolio.profit > 0) "+" else ""}${portfolio.profit.toFormattedString()} 원(${if (portfolio.profitRatio > 0) "+" else ""}${(portfolio.profitRatio * 100).toInt()}%)",
                    fontSize = 16.sp,
                    color = if (portfolio.profit > 0) Primary500 else Secondary500,
                    fontWeight = FontWeight.Bold,
                    fontFamily = SCDreamFontFamily
                )
            }
        }
    }
}

// 숫자를 천 단위 콤마가 있는 포맷으로 변환
fun Int.toFormattedString(): String {
    return String.format("%,d", this)
}

// 포트폴리오 탭 내용
@Composable
fun PortfolioTabContent(
    portfolios: List<Portfolio>,
    totalAsset: Int,
    availableAsset: Int,
    investedAsset: Int,
    totalProfit: Float
) {
    LazyColumn(
        modifier = Modifier.fillMaxWidth()
    ) {
        // 자산 정보 카드
        item {
            Spacer(modifier = Modifier.height(16.dp))
            
            Card(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(horizontal = 16.dp),
                colors = CardDefaults.cardColors(
                    containerColor = Color.White
                )
            ) {
                Column(
                    modifier = Modifier
                        .fillMaxWidth()
                        .padding(16.dp)
                ) {
                    // 자산 정보 행
                    AssetInfoRow("총 자산", "${totalAsset.toFormattedString()} 원")
                    Spacer(modifier = Modifier.height(8.dp))
                    AssetInfoRow("가용 자산", "${availableAsset.toFormattedString()} 원")
                    Spacer(modifier = Modifier.height(8.dp))
                    AssetInfoRow("투자 자산", "${investedAsset.toFormattedString()} 원")
                    Spacer(modifier = Modifier.height(8.dp))
                    AssetInfoRow(
                        "수익률", 
                        "0.03%(+1000 원)", 
                        color = Primary500
                    )
                    
                    Spacer(modifier = Modifier.height(16.dp))
                    
                    // 파이 차트
                    Text(
                        text = "보유 주식",
                        fontSize = 16.sp,
                        fontWeight = FontWeight.Bold,
                        fontFamily = SCDreamFontFamily
                    )
                    
                    Spacer(modifier = Modifier.height(16.dp))
                    
                    Box(
                        modifier = Modifier
                            .fillMaxWidth()
                            .height(160.dp),
                        contentAlignment = Alignment.Center
                    ) {
                        AccountBookGraph(
                            modifier = Modifier.fillMaxWidth(),
                            colors = listOf(
                                Color(0xFF3F81F0), // 삼성전자 색상
                                Color(0xFFFFCA29)  // 한화오션 색상
                            ),
                            data = listOf(
                                portfolios[0].ratio,
                                portfolios[1].ratio
                            ),
                            graphHeight = 140
                        )
                    }
                    
                    Spacer(modifier = Modifier.height(16.dp))
                    
                    // 차트 범례
                    Row(
                        modifier = Modifier.fillMaxWidth(),
                        horizontalArrangement = Arrangement.SpaceEvenly
                    ) {
                        ChartLegendItem(
                            color = Color(0xFF3F81F0),
                            label = portfolios[0].stockName,
                            value = "${(portfolios[0].ratio * 100).toInt()}.${((portfolios[0].ratio * 1000) % 10).toInt()}%"
                        )
                        ChartLegendItem(
                            color = Color(0xFFFFCA29),
                            label = portfolios[1].stockName,
                            value = "${(portfolios[1].ratio * 100).toInt()}.${((portfolios[1].ratio * 1000) % 10).toInt()}%"
                        )
                    }
                }
            }
        }
        
        // 포트폴리오 아이템들
        item {
            Spacer(modifier = Modifier.height(16.dp))
            PortfolioItem(portfolio = portfolios[0])
        }
        
        item {
            PortfolioItem(portfolio = portfolios[1])
        }
        
        // 하단 여백
        item {
            Spacer(modifier = Modifier.height(16.dp))
        }
    }
}

// 거래 기록 탭 내용
@Composable
fun TradeHistoryTabContent(tradeRecords: List<TradeRecord>) {
    LazyColumn(
        modifier = Modifier.fillMaxWidth(),
        contentPadding = PaddingValues(16.dp)
    ) {
        items(tradeRecords.size) { index ->
            TradeHistoryItem(record = tradeRecords[index])
            
            if (index < tradeRecords.size - 1) {
                Spacer(modifier = Modifier.height(12.dp))
            }
        }
    }
}

// 관심 종목 탭 내용
@Composable
fun FavoriteStocksTabContent(
    favoriteStocks: List<FavoriteStock>,
    navController: NavController
) {
    LazyColumn(
        modifier = Modifier.fillMaxWidth(),
        contentPadding = PaddingValues(16.dp)
    ) {
        items(favoriteStocks.size) { index ->
            FavoriteStockItem(
                stock = favoriteStocks[index],
                onFavoriteClick = { /* 즐겨찾기 제거 처리 */ },
                onItemClick = { /* 종목 상세 화면으로 이동 */ }
            )
            
            if (index < favoriteStocks.size - 1) {
                Spacer(modifier = Modifier.height(12.dp))
            }
        }
    }
}

// 거래 타입 (매수/매도)
enum class TradeType { BUY, SELL }

// 거래 기록 데이터 클래스
data class TradeRecord(
    val stockName: String,
    val stockLogo: Int,
    val tradeType: TradeType,
    val price: Int,
    val quantity: Int,
    val date: String,
    val time: String
)

// 관심 종목 데이터 클래스
data class FavoriteStock(
    val stockName: String,
    val stockLogo: Int,
    val currentPrice: Int,
    val fluctuationRate: Float
)

// 거래 기록 아이템 UI
@Composable
fun TradeHistoryItem(record: TradeRecord) {
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .padding(vertical = 8.dp),
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.SpaceBetween
    ) {
        if (record.tradeType == TradeType.SELL) {  // SELL 타입일 때 (매도)
            // 매도의 경우 - 왼쪽: 박스, 오른쪽: 날짜
            // 왼쪽: 컨테이너
            Card(
                shape = RoundedCornerShape(16.dp),
                colors = CardDefaults.cardColors(containerColor = Color.Transparent),
                elevation = CardDefaults.cardElevation(defaultElevation = 0.dp),
                modifier = Modifier
                    .weight(1f)
                    .padding(end = 8.dp)
            ) {
                Row(
                    modifier = Modifier
                        .fillMaxWidth()
                        .height(72.dp),  // 고정 높이 설정
                    verticalAlignment = Alignment.CenterVertically
                ) {
                    // 로고, 종목명 부분 (왼쪽)
                    Row(
                        modifier = Modifier
                            .weight(1f)
                            .background(Color(0xFFF5F5F5))
                            .height(72.dp)  // 고정 높이 설정
                            .padding(horizontal = 16.dp),
                        verticalAlignment = Alignment.CenterVertically
                    ) {
                        // 종목 로고
                        AsyncImage(
                            model = "https://thumb.tossinvest.com/image/resized/96x0/https%3A%2F%2Fstatic.toss.im%2Fpng-icons%2Fsecurities%2Ficn-sec-fill-005930.png",
                            contentDescription = "주식 로고",
                            modifier = Modifier
                                .size(30.dp)
                                .clip(CircleShape)
                                .background(Color.White)
                        )
                        
                        // 종목 이름
                        Text(
                            text = record.stockName,
                            fontSize = 12.sp,
                            fontWeight = FontWeight.W700,
                            fontFamily = SCDreamFontFamily,
                            modifier = Modifier.padding(start = 8.dp)
                        )
                    }
                    
                    // 빨간색 부분 (매수가 스타일로 표시)
                    Box(
                        modifier = Modifier
                            .background(Color(0xFFFF6B6B))
                            .height(72.dp)  // 고정 높이 설정
                            .padding(horizontal = 16.dp)
                            .clip(RoundedCornerShape(topEnd = 16.dp, bottomEnd = 16.dp)),
                        contentAlignment = Alignment.Center
                    ) {
                        Column(
                            horizontalAlignment = Alignment.CenterHorizontally
                        ) {
                            Text(
                                text = "매도가 ${record.price.toFormattedString()}원",
                                fontSize = 12.sp,
                                color = Color.White,
                                fontFamily = SCDreamFontFamily,
                                fontWeight = FontWeight.Bold
                            )
                            
                            Text(
                                text = "${record.quantity}주",
                                fontSize = 12.sp,
                                color = Color.White,
                                fontFamily = SCDreamFontFamily,
                                fontWeight = FontWeight.Bold
                            )
                        }
                    }
                }
            }
            
            // 오른쪽: 날짜 시간
            Column(
                horizontalAlignment = Alignment.End,
                modifier = Modifier.width(100.dp)
            ) {
                Text(
                    text = record.date,
                    fontSize = 12.sp,
                    color = Color.Gray,
                    fontFamily = SCDreamFontFamily
                )
                Text(
                    text = record.time,
                    fontSize = 12.sp,
                    color = Color.Gray,
                    fontFamily = SCDreamFontFamily
                )
            }
        } else {  // BUY 타입일 때 (매수)
            // 매수의 경우 - 왼쪽: 날짜, 오른쪽: 박스
            // 왼쪽: 날짜 시간
            Column(
                horizontalAlignment = Alignment.Start,
                modifier = Modifier.width(100.dp)
            ) {
                Text(
                    text = record.date,
                    fontSize = 12.sp,
                    color = Color.Gray,
                    fontFamily = SCDreamFontFamily
                )
                Text(
                    text = record.time,
                    fontSize = 12.sp,
                    color = Color.Gray,
                    fontFamily = SCDreamFontFamily
                )
            }
            
            // 오른쪽: 컨테이너
            Card(
                shape = RoundedCornerShape(16.dp),
                colors = CardDefaults.cardColors(containerColor = Color.Transparent),
                elevation = CardDefaults.cardElevation(defaultElevation = 0.dp),
                modifier = Modifier
                    .weight(1f)
                    .padding(start = 8.dp)
            ) {
                Row(
                    modifier = Modifier
                        .fillMaxWidth()
                        .height(72.dp),  // 고정 높이 설정
                    verticalAlignment = Alignment.CenterVertically
                ) {
                    // 파란색 부분 (매도가 스타일로 표시)
                    Box(
                        modifier = Modifier
                            .background(Color(0xFF4DABF7))
                            .height(72.dp)  // 고정 높이 설정
                            .padding(horizontal = 16.dp)
                            .clip(RoundedCornerShape(topStart = 16.dp, bottomStart = 16.dp)),
                        contentAlignment = Alignment.Center
                    ) {
                        Column(
                            horizontalAlignment = Alignment.CenterHorizontally
                        ) {
                            Text(
                                text = "매수가 ${record.price.toFormattedString()}원",
                                fontSize = 12.sp,
                                color = Color.White,
                                fontFamily = SCDreamFontFamily,
                                fontWeight = FontWeight.Bold
                            )
                            
                            Text(
                                text = "${record.quantity}주",
                                fontSize = 12.sp,
                                color = Color.White,
                                fontFamily = SCDreamFontFamily,
                                fontWeight = FontWeight.Bold
                            )
                        }
                    }
                    
                    // 로고, 종목명 부분 (오른쪽)
                    Row(
                        modifier = Modifier
                            .weight(1f)
                            .background(Color(0xFFF5F5F5))
                            .height(72.dp)  // 고정 높이 설정
                            .padding(horizontal = 16.dp),
                        verticalAlignment = Alignment.CenterVertically,
                        horizontalArrangement = Arrangement.End
                    ) {
                        // 종목 로고
                        AsyncImage(
                            model = "https://thumb.tossinvest.com/image/resized/96x0/https%3A%2F%2Fstatic.toss.im%2Fpng-icons%2Fsecurities%2Ficn-sec-fill-005930.png",
                            contentDescription = "주식 로고",
                            modifier = Modifier
                                .size(30.dp)
                                .clip(CircleShape)
                                .background(Color.White)
                        )
                        
                        // 종목 이름
                        Text(
                            text = record.stockName,
                            fontSize = 12.sp,
                            fontWeight = FontWeight.W700,
                            fontFamily = SCDreamFontFamily,
                            modifier = Modifier.padding(start = 8.dp)
                        )
                    }
                }
            }
        }
    }
}

// 관심 종목 아이템 UI
@Composable
fun FavoriteStockItem(
    stock: FavoriteStock,
    onFavoriteClick: () -> Unit,
    onItemClick: () -> Unit
) {
    val isNegative = stock.fluctuationRate < 0
    val fluctuationColor = if (isNegative) Color.Blue else Color.Red
    
    Surface(
        modifier = Modifier
            .fillMaxWidth()
            .padding(vertical = 4.dp)
            .clickable(onClick = onItemClick),
        shape = RoundedCornerShape(16.dp),
        color = Color(0xFFF8F8F8),
        shadowElevation = 2.dp
    ) {
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .padding(12.dp),
            horizontalArrangement = Arrangement.SpaceBetween,
            verticalAlignment = Alignment.CenterVertically
        ) {
            // 왼쪽 정렬: 로고, 이름&금액
            Row(
                verticalAlignment = Alignment.CenterVertically,
                modifier = Modifier.weight(1f)
            ) {
                // 종목 로고
                AsyncImage(
                    model = "https://thumb.tossinvest.com/image/resized/96x0/https%3A%2F%2Fstatic.toss.im%2Fpng-icons%2Fsecurities%2Ficn-sec-fill-005930.png",
                    contentDescription = "주식 로고",
                    modifier = Modifier
                        .size(40.dp)
                        .clip(CircleShape)
                        .background(Color.White)
                )
                
                // 회사 이름과 가격
                Column(
                    modifier = Modifier.padding(start = 8.dp)
                ) {
                    Text(
                        text = stock.stockName,
                        fontSize = 14.sp,
                        fontWeight = FontWeight.W700,
                        fontFamily = SCDreamFontFamily
                    )
                    Row(verticalAlignment = Alignment.CenterVertically) {
                        Text(
                            text = "${stock.currentPrice.toFormattedString()}원",
                            fontSize = 12.sp,
                            fontWeight = FontWeight.W500,
                            fontFamily = SCDreamFontFamily
                        )
                        Text(
                            text = " ${if (isNegative) "" else "+"}${stock.fluctuationRate}%",
                            fontSize = 12.sp,
                            fontWeight = FontWeight.W500,
                            color = fluctuationColor,
                            fontFamily = SCDreamFontFamily
                        )
                    }
                }
            }
            
            // 하트 아이콘
            IconButton(
                onClick = onFavoriteClick,
                modifier = Modifier.size(22.dp)
            ) {
                Icon(
                    imageVector = Icons.Outlined.FavoriteBorder,
                    contentDescription = "즐겨찾기",
                    tint = Gray300,
                    modifier = Modifier.size(22.dp)
                )
            }
        }
    }
} 