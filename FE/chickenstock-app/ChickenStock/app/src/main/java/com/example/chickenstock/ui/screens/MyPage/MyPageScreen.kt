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
import androidx.compose.material.icons.filled.Favorite
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
import androidx.compose.ui.platform.LocalContext
import androidx.lifecycle.viewmodel.compose.viewModel
import com.example.chickenstock.viewmodel.AuthViewModel
import com.example.chickenstock.navigation.Screen
import android.util.Log
import com.example.chickenstock.api.RetrofitClient
import com.example.chickenstock.api.MemberService
import com.example.chickenstock.api.SimpleProfileResponse
import com.example.chickenstock.api.AssetAllocationResponse

// 새로운 포트폴리오 데이터 클래스들 추가
data class PortfolioData(
    val memberMoney: Int,          // 현금 보유액
    val totalAsset: Int,           // 총 자산 (현금 + 주식 평가금액)
    val totalInvestment: Int,      // 총 투자금액 (주식 매입에 사용한 금액)
    val totalValuation: Int,       // 보유 주식 총 평가금액
    val totalProfitLoss: Int,      // 총 손익 (평가금액 - 투자금액)
    val totalReturnRate: Float,    // 총 수익률 (%)
    val positions: List<Position>, // 보유 주식 목록
    val updatedAt: String          // 데이터 갱신 시간
)

data class Position(
    val stockCode: String,      // 종목코드
    val stockName: String,      // 종목명
    val quantity: Int,          // 보유 수량
    val averagePrice: Int,      // 평균 매입가
    val currentPrice: Int,      // 현재가
    val valuationAmount: Int,   // 평가금액 (현재가 × 수량)
    val profitLoss: Int,        // 손익 (평가금액 - 매입금액)
    val returnRate: Float       // 수익률 (%)
)

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun MyPageScreen(
    navController: NavController,
    authViewModel: AuthViewModel = viewModel(factory = AuthViewModel.Factory(LocalContext.current)),
    initialTab: String? = null  // 초기 탭 파라미터 추가
) {
    val context = LocalContext.current
    // API 상태 관리
    var userProfile by remember { mutableStateOf<SimpleProfileResponse?>(null) }
    var assetAllocation by remember { mutableStateOf<AssetAllocationResponse?>(null) }
    var isLoading by remember { mutableStateOf(true) }
    var error by remember { mutableStateOf<String?>(null) }

    // API 호출
    LaunchedEffect(Unit) {
        if (authViewModel.isLoggedIn.value) {
            try {
                val memberService = RetrofitClient.getInstance(context).create(MemberService::class.java)
                
                // 프로필 정보 가져오기
                val profileResponse = memberService.getSimpleProfile()
                if (profileResponse.isSuccessful) {
                    userProfile = profileResponse.body()
                    Log.d("MyPageScreen", "프로필 로드 성공: ${userProfile?.nickname}")
                } else {
                    error = "프로필 정보를 불러오는데 실패했습니다."
                    Log.e("MyPageScreen", "프로필 로드 실패: ${profileResponse.code()}")
                }

                // 자산 비중 정보 가져오기
                val assetResponse = memberService.getAssetAllocation()
                if (assetResponse.isSuccessful) {
                    assetAllocation = assetResponse.body()
                    Log.d("MyPageScreen", "자산 정보 로드 성공: ${assetAllocation?.totalAsset}")
                } else {
                    error = "자산 정보를 불러오는데 실패했습니다."
                    Log.e("MyPageScreen", "자산 정보 로드 실패: ${assetResponse.code()}")
                }
            } catch (e: Exception) {
                error = "네트워크 오류가 발생했습니다."
                Log.e("MyPageScreen", "API 호출 실패", e)
            } finally {
                isLoading = false
            }
        }
    }

    // 초기 탭 인덱스 설정
    var selectedTabIndex by remember(initialTab) { 
        mutableStateOf(
            when (initialTab) {
                "portfolio" -> 0
                "history" -> 1
                "favorite" -> 2
                null -> 0
                else -> 0
            }
        )
    }

    // 그래프 애니메이션 상태를 상위 컴포넌트에서 관리
    val hasAnimated = remember { mutableStateOf(false) }
    
    // 샘플 포트폴리오 데이터
    val portfolioData = remember {
        PortfolioData(
            memberMoney = 5000000,     // 현금 보유액
            totalAsset = 15200000,     // 총 자산 (현금 + 주식 평가금액)
            totalInvestment = 10100000, // 총 투자금액 (주식 매입에 사용한 금액)
            totalValuation = 10200000, // 보유 주식 총 평가금액
            totalProfitLoss = 100000,  // 총 손익 (평가금액 - 투자금액)
            totalReturnRate = 0.99f,   // 총 수익률 (%)
            positions = listOf(        // 보유 주식 목록
                Position(
                    stockCode = "005930",     // 종목코드
                    stockName = "삼성전자",      // 종목명
                    quantity = 100,           // 보유 수량
                    averagePrice = 75000,     // 평균 매입가
                    currentPrice = 76000,     // 현재가
                    valuationAmount = 7600000,// 평가금액 (현재가 × 수량)
                    profitLoss = 100000,      // 손익 (평가금액 - 매입금액)
                    returnRate = 1.33f        // 수익률 (%)
                ),
                Position(
                    stockCode = "000660",     // 종목코드
                    stockName = "SK하이닉스",   // 종목명
                    quantity = 20,            // 보유 수량
                    averagePrice = 130000,    // 평균 매입가
                    currentPrice = 130000,    // 현재가
                    valuationAmount = 2600000,// 평가금액 (현재가 × 수량)
                    profitLoss = 0,           // 손익 (평가금액 - 매입금액)
                    returnRate = 0.0f         // 수익률 (%)
                )
            ),
            updatedAt = "2025-04-28T15:30:25" // 데이터 갱신 시간
        )
    }

    val angleAnimatables = remember {
        if (portfolioData.positions.isNotEmpty()) {
            val total = portfolioData.positions.sumOf { it.valuationAmount }.toFloat()
            portfolioData.positions.map { position ->
                Animatable(0f) to (position.valuationAmount.toFloat() / total * 360f)
            }
        } else {
            emptyList()
        }
    }

    // 처음 한 번만 애니메이션 실행
    LaunchedEffect(Unit) {
        if (!hasAnimated.value && angleAnimatables.isNotEmpty()) {
            angleAnimatables.forEach { (animatable, targetAngle) ->
                launch {
                    animatable.animateTo(
                        targetValue = targetAngle,
                        animationSpec = tween(
                            durationMillis = 1000,
                            easing = LinearOutSlowInEasing
                        )
                    )
                }
            }
            hasAnimated.value = true
        }
    }

    val tabs = listOf("포트폴리오", "거래 기록", "관심 종목")
    
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
                // 설정 아이콘 (로그인된 경우에만 표시)
                if (authViewModel.isLoggedIn.value) {
                    IconButton(
                        onClick = { 
                            navController.navigate("setting") {
                                launchSingleTop = true
                            }
                        },
                        modifier = Modifier
                            .align(Alignment.TopEnd)
                            .offset(y = (-8).dp)
                    ) {
                        Icon(
                            imageVector = Icons.Default.Settings,
                            contentDescription = "설정",
                            tint = Color.Black
                        )
                    }
                }
                
                // 프로필 정보
                Column(
                    horizontalAlignment = Alignment.CenterHorizontally,
                    modifier = Modifier.fillMaxWidth()
                ) {
                    if (authViewModel.isLoggedIn.value) {
                        // 로그인된 상태
                        if (isLoading) {
                            CircularProgressIndicator(
                                modifier = Modifier.size(48.dp),
                                color = Primary500
                            )
                        } else {
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
                                text = userProfile?.nickname ?: "사용자",
                                fontSize = 18.sp,
                                fontWeight = FontWeight.Bold,
                                fontFamily = SCDreamFontFamily
                            )
                        }
                    } else {
                        // 로그인되지 않은 상태
                        Spacer(modifier = Modifier.height(20.dp))
                        
                        Text(
                            text = "서비스를 이용하기 위해\n로그인이 필요합니다",
                            fontSize = 16.sp,
                            fontWeight = FontWeight.Bold,
                            fontFamily = SCDreamFontFamily,
                            textAlign = TextAlign.Center,
                            color = Color.Black
                        )
                        
                        Spacer(modifier = Modifier.height(16.dp))
                        
                        Button(
                            onClick = { 
                                navController.navigate(Screen.Login.route) {
                                    launchSingleTop = true
                                }
                            },
                            colors = ButtonDefaults.buttonColors(containerColor = Color(0xFFFFEB3B)),
                            modifier = Modifier
                                .width(200.dp)
                                .height(48.dp),
                            shape = RoundedCornerShape(12.dp)
                        ) {
                            Text(
                                "로그인하기",
                                color = Color.Black,
                                fontWeight = FontWeight.Bold,
                                fontFamily = SCDreamFontFamily,
                                fontSize = 16.sp
                            )
                        }
                        
                        Spacer(modifier = Modifier.height(20.dp))
                    }
                }
            }
            
            if (authViewModel.isLoggedIn.value) {
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
                    0 -> {
                        if (isLoading) {
                            Box(
                                modifier = Modifier.fillMaxSize(),
                                contentAlignment = Alignment.Center
                            ) {
                                CircularProgressIndicator(
                                    color = Primary500
                                )
                            }
                        } else if (error != null) {
                            Box(
                                modifier = Modifier.fillMaxSize(),
                                contentAlignment = Alignment.Center
                            ) {
                                Text(
                                    text = error ?: "오류가 발생했습니다.",
                                    color = Color.Red
                                )
                            }
                        } else {
                            PortfolioTabContent(
                                totalAsset = assetAllocation?.totalAsset ?: 0,
                                cashAmount = assetAllocation?.cashAmount ?: 0,
                                stockValuation = assetAllocation?.stockValuation ?: 0,
                                returnRate = userProfile?.returnRate?.toFloatOrNull() ?: 0f,
                                hasAnimated = hasAnimated,
                                angleAnimatables = angleAnimatables
                            )
                        }
                    }
                    1 -> TradeHistoryTabContent(tradeRecords = tradeRecords)
                    2 -> FavoriteStocksTabContent(
                        favoriteStocks = favoriteStocks,
                        navController = navController
                    )
                }
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
    angleAnimatables: List<Pair<Animatable<Float, AnimationVector1D>, Float>>,
    graphHeight: Int
) {
    Canvas(modifier = modifier.height(graphHeight.dp)) {
        val strokeWidth = graphHeight.dp.toPx() / 4
        val radius = (graphHeight.dp.toPx() - strokeWidth) / 2
        val centerX = size.width / 2f
        val centerY = radius + strokeWidth / 2

        if (angleAnimatables.isNotEmpty()) {
            var startAngle = -90f
            angleAnimatables.forEachIndexed { index, (animatable, _) ->
                val color = if (index < colors.size) colors[index] else Color.Gray
                val sweepAngle = animatable.value

                drawArc(
                    color = color,
                    startAngle = startAngle,
                    sweepAngle = sweepAngle,
                    useCenter = false,
                    style = Stroke(width = strokeWidth),
                    topLeft = Offset(centerX - radius, centerY - radius),
                    size = Size(radius * 2, radius * 2)
                )

                startAngle += sweepAngle
            }
        } else {
            drawCircle(
                color = Color.Gray.copy(alpha = 0.3f),
                radius = radius,
                center = Offset(centerX, centerY),
                style = Stroke(width = strokeWidth)
            )
        }
    }
}

// 포트폴리오 아이템 UI
@Composable
fun PortfolioItem(position: Position, totalValuation: Int) {
    Card(
        modifier = Modifier
            .fillMaxWidth()
            .padding(horizontal = 32.dp, vertical = 12.dp),
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
                            .background(if (position.stockName == "삼성전자") Color(0xFF1428A0) else Color(0xFFFF8A00))
                            .padding(8.dp),
                        contentAlignment = Alignment.Center
                    ) {
                        Text(
                            text = position.stockName.first().toString(),
                            color = Color.White,
                            fontWeight = FontWeight.Bold
                        )
                    }
                    
                    Spacer(modifier = Modifier.width(12.dp))
                    
                    // 주식 이름
                    Text(
                        text = position.stockName,
                        fontSize = 16.sp,
                        fontWeight = FontWeight.Bold,
                        fontFamily = SCDreamFontFamily
                    )
                }
                
                // 보유 수량
                Text(
                    text = "${position.quantity}주",
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
                    text = "평균 매수 금액: ${position.averagePrice.toFormattedString()} 원",
                    fontSize = 14.sp,
                    color = Color.Gray,
                    fontFamily = SCDreamFontFamily
                )
                
                Spacer(modifier = Modifier.height(4.dp))
                
                Text(
                    text = "현재 주식 가치: ${position.currentPrice.toFormattedString()} 원",
                    fontSize = 14.sp,
                    color = Color.Gray,
                    fontFamily = SCDreamFontFamily
                )
            }
            
            Spacer(modifier = Modifier.height(12.dp))
            
            // 자산 비중 (계산)
            val ratio = position.valuationAmount.toFloat() / totalValuation
            Text(
                text = "전체 자산 중 ${(ratio * 100).toInt()}.${((ratio * 1000) % 10).toInt()}%",
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
                // 평가금액
                Text(
                    text = "${position.valuationAmount.toFormattedString()} 원",
                    fontSize = 18.sp,
                    fontWeight = FontWeight.Bold,
                    fontFamily = SCDreamFontFamily
                )
                // 손익
                Text(
                    text = "${if (position.profitLoss > 0) "+" else ""}${position.profitLoss.toFormattedString()} 원(${if (position.returnRate > 0) "+" else ""}${(position.returnRate * 100).toInt()}%)",
                    fontSize = 16.sp,
                    color = if (position.profitLoss > 0) Primary500 else Secondary500,
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
    totalAsset: Int,
    cashAmount: Int,
    stockValuation: Int,
    returnRate: Float,
    hasAnimated: MutableState<Boolean>,
    angleAnimatables: List<Pair<Animatable<Float, AnimationVector1D>, Float>>
) {
    // 차트 컬러 리스트 (최대 10개까지 지원)
    val chartColors = listOf(
        Color(0xFF3F81F0), // 파란색
        Color(0xFFFFCA29), // 노란색
        Color(0xFF4CAF50), // 초록색
        Color(0xFFFF5722), // 주황색
        Color(0xFF9C27B0), // 보라색
        Color(0xFF03A9F4), // 하늘색
        Color(0xFFE91E63), // 분홍색
        Color(0xFF795548), // 갈색
        Color(0xFF607D8B), // 회색
        Color(0xFF009688)  // 청록색
    )
    
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
                    AssetInfoRow("가용 자산", "${cashAmount.toFormattedString()} 원")
                    Spacer(modifier = Modifier.height(8.dp))
                    AssetInfoRow("투자 자산", "${stockValuation.toFormattedString()} 원")
                    Spacer(modifier = Modifier.height(8.dp))
                    AssetInfoRow(
                        "수익률", 
                        "${returnRate}%", 
                        color = if (returnRate >= 0) Primary500 else Secondary500
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
                    
                    // 포지션이 있는 경우에만 차트 표시
                    if (portfolioData.positions.isNotEmpty()) {
                        Box(
                            modifier = Modifier
                                .fillMaxWidth()
                                .height(160.dp),
                            contentAlignment = Alignment.Center
                        ) {
                            AccountBookGraph(
                                modifier = Modifier.fillMaxWidth(),
                                colors = chartColors.take(portfolioData.positions.size),
                                angleAnimatables = angleAnimatables,
                                graphHeight = 140
                            )
                        }
                        
                        Spacer(modifier = Modifier.height(16.dp))
                        
                        // 차트 범례
                        if (portfolioData.positions.size <= 3) {
                            // 3개 이하면 가로로 배치
                            Row(
                                modifier = Modifier.fillMaxWidth(),
                                horizontalArrangement = Arrangement.SpaceEvenly
                            ) {
                                portfolioData.positions.forEachIndexed { index, position ->
                                    val color = if (index < chartColors.size) chartColors[index] else Color.Gray
                                    val ratio = position.valuationAmount.toFloat() / portfolioData.totalValuation
                                    ChartLegendItem(
                                        color = color,
                                        label = position.stockName,
                                        value = "${(ratio * 100).toInt()}.${((ratio * 1000) % 10).toInt()}%"
                                    )
                                }
                            }
                        } else {
                            // 4개 이상이면 2줄로 나눠서 배치
                            Column {
                                Row(
                                    modifier = Modifier.fillMaxWidth(),
                                    horizontalArrangement = Arrangement.SpaceEvenly
                                ) {
                                    portfolioData.positions.take(2).forEachIndexed { index, position ->
                                        val color = if (index < chartColors.size) chartColors[index] else Color.Gray
                                        val ratio = position.valuationAmount.toFloat() / portfolioData.totalValuation
                                        ChartLegendItem(
                                            color = color,
                                            label = position.stockName,
                                            value = "${(ratio * 100).toInt()}.${((ratio * 1000) % 10).toInt()}%"
                                        )
                                    }
                                }
                                
                                Spacer(modifier = Modifier.height(8.dp))
                                
                                Row(
                                    modifier = Modifier.fillMaxWidth(),
                                    horizontalArrangement = Arrangement.SpaceEvenly
                                ) {
                                    portfolioData.positions.drop(2).take(2).forEachIndexed { index, position ->
                                        val realIndex = index + 2
                                        val color = if (realIndex < chartColors.size) chartColors[realIndex] else Color.Gray
                                        val ratio = position.valuationAmount.toFloat() / portfolioData.totalValuation
                                        ChartLegendItem(
                                            color = color,
                                            label = position.stockName,
                                            value = "${(ratio * 100).toInt()}.${((ratio * 1000) % 10).toInt()}%"
                                        )
                                    }
                                }
                            }
                        }
                    } else {
                        // 포지션이 없는 경우
                        Box(
                            modifier = Modifier
                                .fillMaxWidth()
                                .height(160.dp),
                            contentAlignment = Alignment.Center
                        ) {
                            Text(
                                text = "보유 주식이 없습니다",
                                color = Color.Gray,
                                fontFamily = SCDreamFontFamily
                            )
                        }
                    }
                }
            }
        }
        
        // 업데이트 시간 표시
        item {
            Row(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(horizontal = 16.dp, vertical = 8.dp),
                horizontalArrangement = Arrangement.End
            ) {
                // 날짜 포맷 변경 (2025-04-28T15:30:25 -> 2025.04.28 15:30:25)
                val formattedDate = portfolioData.updatedAt
                    .replace("T", " ")
                    .replace("-", ".")
                
                Text(
                    text = "마지막 업데이트: $formattedDate",
                    fontSize = 12.sp,
                    color = Color.Gray,
                    fontFamily = SCDreamFontFamily
                )
            }
        }
        
        // 포트폴리오 아이템들
        items(portfolioData.positions.size) { index ->
            PortfolioItem(
                position = portfolioData.positions[index], 
                totalValuation = portfolioData.totalValuation
            )
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
    var stocks by remember { mutableStateOf(favoriteStocks) }

    LazyColumn(
        modifier = Modifier.fillMaxWidth(),
        contentPadding = PaddingValues(16.dp)
    ) {
        items(stocks.size) { index ->
            FavoriteStockItem(
                stock = stocks[index],
                onFavoriteClick = {
                    // 리스트에서 해당 항목 제거
                    stocks = stocks.filterIndexed { i, _ -> i != index }
                },
                onItemClick = { /* 종목 상세 화면으로 이동 */ }
            )
            
            if (index < stocks.size - 1) {
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
                        .height(50.dp),  // 고정 높이 설정
                    verticalAlignment = Alignment.CenterVertically
                ) {
                    // 로고, 종목명 부분 (왼쪽)
                    Row(
                        modifier = Modifier
                            .weight(1f)
                            .background(Color(0xFFF5F5F5))
                            .height(50.dp)  // 고정 높이 설정
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
                            fontSize = 10.sp,
                            fontWeight = FontWeight.W700,
                            fontFamily = SCDreamFontFamily,
                            modifier = Modifier.padding(start = 8.dp)
                        )
                    }
                    
                    // 빨간색 부분 (매수가 스타일로 표시)
                    Box(
                        modifier = Modifier
                            .background(Color(0xFFFF6B6B))
                            .padding(horizontal = 16.dp)
                            .clip(RoundedCornerShape(topEnd = 16.dp, bottomEnd = 16.dp)),
                        contentAlignment = Alignment.Center
                    ) {
                        Column(
                            horizontalAlignment = Alignment.CenterHorizontally
                        ) {
                            Text(
                                text = "매도가 ${record.price.toFormattedString()}원",
                                fontSize = 8.sp,
                                color = Color.White,
                                fontFamily = SCDreamFontFamily,
                                fontWeight = FontWeight.Bold
                            )
                            
                            Text(
                                text = "${record.quantity}주",
                                fontSize = 8.sp,
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
                modifier = Modifier.width(70.dp)
            ) {
                Text(
                    text = record.date,
                    fontSize = 10.sp,
                    color = Color.Gray,
                    fontFamily = SCDreamFontFamily
                )
                Text(
                    text = record.time,
                    fontSize = 10.sp,
                    color = Color.Gray,
                    fontFamily = SCDreamFontFamily
                )
            }
        } else {  // BUY 타입일 때 (매수)
            // 매수의 경우 - 왼쪽: 날짜, 오른쪽: 박스
            // 왼쪽: 날짜 시간
            Column(
                horizontalAlignment = Alignment.Start,
                modifier = Modifier.width(70.dp)
            ) {
                Text(
                    text = record.date,
                    fontSize = 10.sp,
                    color = Color.Gray,
                    fontFamily = SCDreamFontFamily
                )
                Text(
                    text = record.time,
                    fontSize = 10.sp,
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
                        .height(50.dp),  // 고정 높이 설정
                    verticalAlignment = Alignment.CenterVertically
                ) {
                    // 파란색 부분 (매도가 스타일로 표시)
                    Box(
                        modifier = Modifier
                            .background(Color(0xFF4DABF7))
                            .height(50.dp)  // 고정 높이 설정
                            .padding(horizontal = 16.dp)
                            .clip(RoundedCornerShape(topStart = 16.dp, bottomStart = 16.dp)),
                        contentAlignment = Alignment.Center
                    ) {
                        Column(
                            horizontalAlignment = Alignment.CenterHorizontally
                        ) {
                            Text(
                                text = "매수가 ${record.price.toFormattedString()}원",
                                fontSize = 8.sp,
                                color = Color.White,
                                fontFamily = SCDreamFontFamily,
                                fontWeight = FontWeight.Bold
                            )
                            
                            Text(
                                text = "${record.quantity}주",
                                fontSize = 8.sp,
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
                            .height(50.dp)  // 고정 높이 설정
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
                            fontSize = 10.sp,
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
            .padding(vertical = 4.dp, horizontal = 20.dp)
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
                    imageVector = Icons.Filled.Favorite,
                    contentDescription = "관심 종목 제거",
                    tint = Color(0xFFFF4081),  // 핑크색
                    modifier = Modifier.size(22.dp)
                )
            }
        }
    }
}

// 자산 비중 API 응답 데이터 클래스 추가
data class AssetAllocationResponse(
    val totalAsset: Int,      // 총 자산(현금+주식)
    val cashAmount: Int,      // 현금 자산
    val stockValuation: Int,  // 주식 평가금액
    val cashRatio: Float,     // 현금 비중(%)
    val stockRatio: Float,    // 주식 비중(%)
    val stocks: List<Any>,    // 기존 코드에서는 개별 종목 없음
    val updatedAt: String     // 정보 업데이트 시간
) 