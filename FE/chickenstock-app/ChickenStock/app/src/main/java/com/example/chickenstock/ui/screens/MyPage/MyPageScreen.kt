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
import androidx.compose.ui.unit.Dp
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
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import retrofit2.http.GET
import retrofit2.http.Header
import retrofit2.Retrofit
import retrofit2.converter.gson.GsonConverterFactory
import com.example.chickenstock.api.RetrofitClient
import com.example.chickenstock.api.MemberService
import android.util.Log
import com.example.chickenstock.model.PortfolioData
import com.example.chickenstock.model.Position
import com.example.chickenstock.model.TradeHistoryResponse
import com.example.chickenstock.viewmodel.MainViewModel
import com.example.chickenstock.model.TradeHistory
import retrofit2.http.Query
import com.example.chickenstock.api.WatchlistItem
import androidx.compose.ui.draw.alpha
import com.example.chickenstock.api.PortfolioWebSocketManager
import android.content.Context
import kotlinx.coroutines.flow.collectLatest

// PortfolioData API 서비스 인터페이스 정의
interface PortfolioService {
    @GET("members/portfolio")
    suspend fun getPortfolio(@Header("Authorization") token: String): PortfolioData
}

interface TradeHistoryService {
    @GET("trade-histories")
    suspend fun getTradeHistories(
        @Header("Authorization") token: String,
        @Query("size") size: Int,
        @Query("cursor") cursor: String? = null
    ): TradeHistoryResponse
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun MyPageScreen(
    navController: NavController,
    authViewModel: AuthViewModel = viewModel(factory = AuthViewModel.Factory(LocalContext.current)),
    mainViewModel: MainViewModel = viewModel(),
    initialTab: String? = null  // 초기 탭 파라미터 추가
) {
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

    val tabs = listOf("포트폴리오", "거래 기록", "관심 종목")
    
    val context = LocalContext.current
    val tokenManager = remember { com.example.chickenstock.data.TokenManager.getInstance(context) }
    var assetData by remember { mutableStateOf<PortfolioData?>(null) }
    var isLoading by remember { mutableStateOf(false) }
    var error by remember { mutableStateOf<String?>(null) }

    // 포트폴리오 웹소켓 매니저 가져오기
    val portfolioWebSocketManager = remember { PortfolioWebSocketManager.getInstance() }
    
    // 웹소켓에서 업데이트된 포트폴리오 데이터 수집
    val portfolioUpdateData by portfolioWebSocketManager.portfolioUpdateFlow.collectAsState()

    // 최종적으로 화면에 표시할 포트폴리오 데이터 (REST API 데이터 또는 웹소켓 업데이트)
    val displayPortfolioData = remember(portfolioUpdateData, assetData) {
        portfolioUpdateData ?: assetData
    }
    
    // 실시간 업데이트 여부 추적
    val isRealtimeUpdated = remember(portfolioUpdateData) {
        portfolioUpdateData != null
    }
    
    // 웹소켓 업데이트 효과 추적
    LaunchedEffect(portfolioUpdateData) {
        if (portfolioUpdateData != null) {
            Log.d("MyPageScreen", "웹소켓에서 업데이트된 포트폴리오 데이터 받음: ${portfolioUpdateData?.positions?.size}개 종목")
        }
    }

    // 서비스 초기화
    LaunchedEffect(authViewModel.isLoggedIn.value) {
        if (authViewModel.isLoggedIn.value) {
            // MainViewModel 서비스 초기화
            mainViewModel.initializeServices(context)
            
            // 사용자 프로필 정보 로드
            val memberService = RetrofitClient.getInstance(context).create(MemberService::class.java)
            mainViewModel.loadUserProfile(memberService, context)
        }
    }

    // API 호출 (로그인 상태일 때만)
    LaunchedEffect(authViewModel.isLoggedIn.value) {
        if (authViewModel.isLoggedIn.value) {
            isLoading = true
            error = null
            try {
                val retrofit = RetrofitClient.getInstance(context)
                val service = retrofit.create(PortfolioService::class.java)
                val token = tokenManager.getAccessToken()
                if (!token.isNullOrBlank()) {
                    assetData = withContext(Dispatchers.IO) {
                        service.getPortfolio("Bearer $token")
                    }
                    
                    // 초기 포트폴리오 데이터를 웹소켓 매니저에 설정
                    assetData?.let { portfolioData ->
                        portfolioWebSocketManager.setInitialPortfolioData(portfolioData)
                        
                        // 웹소켓 연결 시작
                        portfolioWebSocketManager.connect(context)
                        Log.d("MyPageScreen", "포트폴리오 웹소켓 연결 시작")
                    }
                }
            } catch (e: Exception) {
                error = e.message
                Log.e("MyPageScreen", "포트폴리오 데이터 로드 오류", e)
            } finally {
                isLoading = false
            }
        }
    }

    // 컴포넌트가 사라질 때 웹소켓 연결 종료
    DisposableEffect(Unit) {
        onDispose {
            Log.d("MyPageScreen", "포트폴리오 웹소켓 연결 종료")
            portfolioWebSocketManager.disconnect()
        }
    }

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
                        
                        // 사용자 이름 - userProfile에서 가져옴
                        val userProfile by mainViewModel.userProfile.collectAsState()
                        Text(
                            text = userProfile?.nickname ?: "사용자",
                            fontSize = 18.sp,
                            fontWeight = FontWeight.Bold,
                            fontFamily = SCDreamFontFamily
                        )
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
                    0 -> PortfolioTabContent(assetData = displayPortfolioData, isRealtimeUpdated = isRealtimeUpdated)
                    1 -> TradeHistoryTabContent(mainViewModel = mainViewModel)
                    2 -> FavoriteStocksTabContent(
                        navController = navController,
                        mainViewModel = mainViewModel,
                        authViewModel = authViewModel
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

// 도넛 그래프 컴포저블을 수정합니다.
@Composable
fun AnimatedDonutChart(
    ratios: List<Double>,
    colors: List<Color>,
    modifier: Modifier = Modifier,
    size: Dp = 120.dp
) {
    // 총합 계산 (전체가 1.0 합이 되도록)
    val total = ratios.sum()
    
    // 총합이 0인 경우 빈 차트를 표시
    if (total <= 0.0) {
        Canvas(modifier = modifier.size(size)) {
            val strokeWidth = size.toPx() / 4
            val radius = (size.toPx() - strokeWidth) / 2
            val center = Offset(size.toPx() / 2, size.toPx() / 2)
            
            // 빈 원 그리기
            drawCircle(
                color = Color.LightGray.copy(alpha = 0.3f),
                radius = radius,
                center = center,
                style = Stroke(width = strokeWidth)
            )
        }
        return
    }
    
    // 비율을 360도로 변환 (이미 정렬된 데이터 사용)
    val angles = ratios.map { (it / total * 360).toFloat() }
    
    // 각 데이터 조각에 대해 Animatable 객체 생성
    val angleAnimatables = remember(angles) { 
        angles.mapIndexed { index, targetAngle ->
            Animatable(0f) to targetAngle  // 초기값 0, 목표값은 계산된 각도
        }
    }
    
    // 애니메이션 적용
    LaunchedEffect(angles) {
        angleAnimatables.forEachIndexed { index, (animatable, targetAngle) ->
            launch {
                // 현재 값에서 목표 값으로 애니메이션 적용
                animatable.animateTo(
                    targetValue = targetAngle,
                    animationSpec = tween(
                        durationMillis = 800,  // 애니메이션 시간 조정
                        easing = FastOutSlowInEasing  // 더 부드러운 이징 함수 사용
                    )
                )
            }
        }
    }
    
    Canvas(modifier = modifier.size(size)) {
        val strokeWidth = size.toPx() / 4
        val radius = (size.toPx() - strokeWidth) / 2
        val center = Offset(size.toPx() / 2, size.toPx() / 2)
        
        // 배경 원 그리기 (시각적 피드백용)
        drawCircle(
            color = Color.LightGray.copy(alpha = 0.1f),
            radius = radius,
            center = center,
            style = Stroke(width = strokeWidth)
        )
        
        var startAngle = -90f // 그래프의 시작 각도 설정
        
        angleAnimatables.forEachIndexed { index, (animatable, _) ->
            val color = colors[index % colors.size]
            val sweepAngle = animatable.value
            
            drawArc(
                color = color,
                startAngle = startAngle,
                sweepAngle = sweepAngle,
                useCenter = false,
                style = Stroke(width = strokeWidth),
                topLeft = Offset(center.x - radius, center.y - radius),
                size = Size(radius * 2, radius * 2)
            )
            
            startAngle += sweepAngle
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
                    // 로고 - AsyncImage로 변경
                    AsyncImage(
                        model = "https://thumb.tossinvest.com/image/resized/96x0/https%3A%2F%2Fstatic.toss.im%2Fpng-icons%2Fsecurities%2Ficn-sec-fill-${position.stockCode}.png",
                        contentDescription = "주식 로고",
                        modifier = Modifier
                            .size(40.dp)
                            .clip(CircleShape)
                            .background(Color.White)
                    )

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
                // 손익 및 수익률
                val profitLoss = position.profitLoss
                val returnRate = position.returnRate
                val isPositive = profitLoss > 0
                val isNegative = profitLoss < 0
                val profitLossColor = when {
                    isPositive -> Color.Red
                    isNegative -> Color.Blue
                    else -> Color.Gray
                }
                val profitLossText = "${if (isPositive) "+" else if (isNegative) "-" else ""}${kotlin.math.abs(profitLoss).toFormattedString()} 원"
                val returnRateText = "(${if (returnRate > 0) "+" else if (returnRate < 0) "-" else ""}${String.format("%.2f", kotlin.math.abs(returnRate))}%)"

                Text(
                    text = "$profitLossText$returnRateText",
                    fontSize = 14.sp,
                    color = profitLossColor,
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

// 숫자 문자열에서 부호를 제거하는 함수
fun String.removeSign(): String {
    return this.replace("""[+\-]""".toRegex(), "")
}

// 포트폴리오 탭 내용
@Composable
fun PortfolioTabContent(
    assetData: PortfolioData?, 
    isRealtimeUpdated: Boolean = false  // 실시간 업데이트 여부
) {
    if (assetData == null) {
        Box(modifier = Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
            Text(text = "데이터가 없습니다.", color = Color.Gray)
        }
        return
    }

    // 초기 데이터 저장 (웹소켓 업데이트와 무관하게 유지)
    val initialData = remember { assetData }
    
    // 도넛 그래프는 초기 데이터를 사용하고, 나머지 정보는 업데이트된 데이터 사용
    val chartData = initialData
    val displayData = assetData
    
    // 포트폴리오 데이터가 변경될 때마다 UI 업데이트를 위해 key 설정
    val updateKey = remember(assetData) { System.currentTimeMillis() }
    
    val totalAsset = displayData.totalAsset
    val cashAmount = displayData.memberMoney
    val stockValuation = displayData.totalValuation
    val updatedAt = displayData.updatedAt
    val totalValuation = displayData.totalValuation
    val positions = displayData.positions
    val profit = displayData.totalProfitLoss
    val returnRate = displayData.totalReturnRate
    val returnRateColor = when {
        returnRate > 0.0 -> Color.Red
        returnRate < 0.0 -> Color.Blue
        else -> Color.Gray
    }

    // 로그 출력: 전체 데이터
    Log.d("PortfolioData", "[업데이트] 총 자산: $totalAsset, 현금: $cashAmount, 주식 가치: $stockValuation")
    Log.d("PortfolioData", "[업데이트] 포지션 개수: ${positions.size}")
    
    // 도넛 그래프 데이터 준비 (초기 데이터 사용)
    // 평가금액을 기준으로 비율 계산 (allocationRatio 대신)
    val stockRatios = if (chartData.positions.isNotEmpty() && chartData.totalAsset > 0) {
        chartData.positions.map { 
            val calculatedRatio = it.valuationAmount.toDouble() / chartData.totalAsset.toDouble()
            if (calculatedRatio.isNaN() || calculatedRatio < 0.0) 0.0 else calculatedRatio
        }
    } else emptyList()
    
    // 현금 비율도 직접 계산 (초기 데이터 사용)
    val calculatedCashRatio = if (chartData.totalAsset > 0) chartData.memberMoney.toDouble() / chartData.totalAsset.toDouble() else 0.0
    val cashRatio = if (calculatedCashRatio.isNaN() || calculatedCashRatio < 0.0) 0.0 else calculatedCashRatio
    
    // 비율이 모두 0이면 기본값 설정 (1:1 비율)
    val ratios = if (stockRatios.isEmpty() && cashRatio <= 0.0) {
        listOf(1.0) // 기본값으로 하나의 항목만 표시
    } else {
        stockRatios + cashRatio
    }
    
    val donutColors = listOf(Color(0xFF1428A0), Color(0xFFFF8A00), Color(0xFF4DABF7), Color(0xFFFF6B6B), Color(0xFF8BC34A), Color(0xFFBDBDBD)) // 마지막이 현금
    val legends = if (chartData.positions.isNotEmpty()) {
        chartData.positions.map { it.stockName } + listOf("현금")
    } else {
        listOf("현금")
    }
    
    // 비율 기준으로 정렬할 인덱스 생성 (비율이 높은 순)
    val safeLegendsSize = minOf(legends.size, ratios.size)
    
    // 인덱스 목록을 비율 기준으로 정렬
    val sortedIndices = (0 until safeLegendsSize).sortedByDescending { ratios[it] }

    // 도넛 차트 키를 고정하여 실시간 업데이트되지 않도록 함
    val chartKey = remember { "chart_fixed_key" }

    LazyColumn(modifier = Modifier.fillMaxWidth()) {
        // key를 이용해 항목이 변경될 때 UI를 강제로 업데이트
        item(key = "header_${updateKey}") {
            Spacer(modifier = Modifier.height(16.dp))
            Card(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(horizontal = 16.dp),
                colors = CardDefaults.cardColors(containerColor = Color.White)
            ) {
                Column(
                    modifier = Modifier
                        .fillMaxWidth()
                        .padding(16.dp)
                ) {
                    AssetInfoRow("총 자산", "${totalAsset.toFormattedString()} 원")
                    Spacer(modifier = Modifier.height(8.dp))
                    AssetInfoRow("가용 자산", "${cashAmount.toFormattedString()} 원")
                    Spacer(modifier = Modifier.height(8.dp))
                    AssetInfoRow("투자 자산", "${stockValuation.toFormattedString()} 원")
                    Spacer(modifier = Modifier.height(8.dp))
                    AssetInfoRow(
                        "수익률",
                        String.format("%.2f%% (%s원)", returnRate, if (profit > 0) "+${profit.toFormattedString()}" else profit.toFormattedString()),
                        color = returnRateColor
                    )
                }
            }
        }
        
        item(key = "updated_${updateKey}") {
            Row(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(horizontal = 16.dp, vertical = 8.dp),
                horizontalArrangement = Arrangement.End
            ) {
                // updatedAt에서 소수점 이하(마이크로초 등) 제거
                val formattedDate = updatedAt
                    .replace("T", " ")
                    .replace("-", ".")
                    .replace(Regex(":\\d{2}\\.\\d+") , "") // ":ss.마이크로초" -> ":ss"로 치환
                
                if (isRealtimeUpdated) {
                    // 실시간 업데이트 중일 때 애니메이션 표시
                    Row(
                        verticalAlignment = Alignment.CenterVertically
                    ) {
                        val infiniteTransition = rememberInfiniteTransition(label = "update_animation")
                        val alpha by infiniteTransition.animateFloat(
                            initialValue = 0.5f,
                            targetValue = 1.0f,
                            animationSpec = infiniteRepeatable(
                                animation = tween(1000),
                                repeatMode = RepeatMode.Reverse
                            ),
                            label = "alpha_animation"
                        )
                        
                        Box(
                            modifier = Modifier
                                .size(8.dp)
                                .background(Color(0xFF4CAF50), CircleShape)
                                .alpha(alpha)
                        )
                        Spacer(modifier = Modifier.width(4.dp))
                        Text(
                            text = "실시간 업데이트 중 • ",
                            fontSize = 12.sp,
                            color = Color(0xFF4CAF50),
                            fontFamily = SCDreamFontFamily,
                            fontWeight = FontWeight.Bold
                        )
                    }
                }
                
                Text(
                    text = "마지막 업데이트: $formattedDate",
                    fontSize = 12.sp,
                    color = Color.Gray,
                    fontFamily = SCDreamFontFamily
                )
            }
        }
        
        item(key = chartKey) {
            if (ratios.isNotEmpty()) {
                Column(
                    modifier = Modifier
                        .fillMaxWidth()
                        .padding(top = 8.dp, bottom = 8.dp),
                    horizontalAlignment = Alignment.CenterHorizontally
                ) {
                    // 도넛 차트 애니메이션
                    AnimatedDonutChart(
                        ratios = ratios,
                        colors = donutColors,
                        modifier = Modifier.align(Alignment.CenterHorizontally),
                        size = 160.dp
                    )
                    Spacer(modifier = Modifier.height(16.dp))
                    
                    // 범례 - 스크롤 가능한 가로 레이아웃으로 변경
                    Box(
                        modifier = Modifier
                            .fillMaxWidth()
                            .padding(horizontal = 8.dp)
                    ) {
                        Column(modifier = Modifier.fillMaxWidth()) {
                            // 스크롤 가능한 범례 목록
                            androidx.compose.foundation.lazy.LazyRow(
                                modifier = Modifier.fillMaxWidth(),
                                contentPadding = PaddingValues(horizontal = 8.dp),
                                horizontalArrangement = if (safeLegendsSize <= 2) 
                                    Arrangement.spacedBy(16.dp, Alignment.CenterHorizontally)
                                else 
                                    Arrangement.spacedBy(16.dp)
                            ) {
                                // 정렬된 인덱스를 사용하여 아이템 표시
                                items(sortedIndices.size) { i ->
                                    val index = sortedIndices[i]
                                    LegendItem(
                                        color = donutColors[index % donutColors.size],
                                        name = legends[index],
                                        ratio = ratios[index]
                                    )
                                }
                            }
                            
                            // 스크롤 힌트 (범례가 3개 이상일 때만 표시)
                            if (safeLegendsSize > 3) {
                                Spacer(modifier = Modifier.height(8.dp))
                                Row(
                                    modifier = Modifier.fillMaxWidth(),
                                    horizontalArrangement = Arrangement.Center
                                ) {
                                    Text(
                                        text = "← 좌우로 스크롤하여 더 보기 →",
                                        fontSize = 12.sp,
                                        color = Color.Gray,
                                        fontFamily = SCDreamFontFamily,
                                        modifier = Modifier.alpha(0.7f)
                                    )
                                }
                            }
                        }
                    }
                }
            }
        }
        
        // 각 포지션 항목에 고유 키 추가
        items(
            items = positions,
            key = { position -> "${position.stockCode}_${updateKey}" }
        ) { position ->
            PortfolioItem(position = position, totalValuation = totalValuation)
        }
    }
}

@Composable
fun LegendItem(color: Color, name: String, ratio: Double) {
    Card(
        modifier = Modifier
            .wrapContentSize()
            .padding(vertical = 4.dp),
        shape = RoundedCornerShape(8.dp),
        colors = CardDefaults.cardColors(
            containerColor = Color(0xFFF8F8F8)
        ),
        elevation = CardDefaults.cardElevation(
            defaultElevation = 1.dp
        )
    ) {
        Row(
            verticalAlignment = Alignment.CenterVertically,
            modifier = Modifier
                .padding(horizontal = 12.dp, vertical = 8.dp)
        ) {
            Box(
                modifier = Modifier
                    .size(16.dp)
                    .background(color, shape = CircleShape)
            )
            Spacer(modifier = Modifier.width(8.dp))
            Column {
                Text(
                    text = name,
                    fontSize = 14.sp,
                    fontWeight = FontWeight.Bold,
                    fontFamily = SCDreamFontFamily
                )
                Text(
                    text = "${if (ratio.isNaN() || ratio <= 0.0) "0.0" else String.format("%.1f", ratio * 100)}%",
                    fontSize = 12.sp,
                    color = Color.Gray,
                    fontFamily = SCDreamFontFamily
                )
            }
        }
    }
}

// 거래 기록 탭 내용
@Composable
fun TradeHistoryTabContent(
    mainViewModel: MainViewModel = viewModel()
) {
    val context = LocalContext.current
    val tradeHistories by mainViewModel.tradeHistoryList.collectAsState()
    val isLoading by mainViewModel.isLoading.collectAsState()
    val error by mainViewModel.error.collectAsState()
    val hasNext by mainViewModel.hasNext.collectAsState()
    val nextCursor by mainViewModel.nextCursor.collectAsState()
    
    // 컴포넌트가 처음 로드될 때 거래 내역 데이터를 불러옴
    LaunchedEffect(Unit) {
        // 서비스 초기화 먼저 실행
        mainViewModel.initializeServices(context)
        mainViewModel.initializeTradeHistoryService(context)
        // 그 다음 거래 내역 로드
        mainViewModel.loadTradeHistories(context, 10)
    }
    
    // 로딩 상태 표시
    if (isLoading && tradeHistories.isEmpty()) {
        Box(modifier = Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
            CircularProgressIndicator(color = Primary500)
        }
        return
    }
    
    // 에러 상태 표시
    if (error != null && tradeHistories.isEmpty()) {
        Box(modifier = Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
            Text(text = "데이터를 불러오는데 실패했습니다.", color = Color.Gray)
        }
        return
    }
    
    // 데이터가 없는 경우 표시
    if (tradeHistories.isEmpty()) {
        Box(modifier = Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
            Text(text = "거래 내역이 없습니다.", color = Color.Gray, fontFamily = SCDreamFontFamily)
        }
        return
    }
    
    // 거래 내역 목록
    LazyColumn(
        modifier = Modifier.fillMaxWidth(),
        contentPadding = PaddingValues(16.dp)
    ) {
        items(tradeHistories.size) { index ->
            TradeHistoryItem(history = tradeHistories[index])
            
            // 마지막 아이템에 도달하면 더 불러오기
            if (index == tradeHistories.size - 1) {
                if (hasNext) {
                    // 다음 페이지가 있으면 로딩 표시 및 추가 데이터 로드
                    LaunchedEffect(nextCursor) {
                        mainViewModel.loadTradeHistories(context, 10, nextCursor)
                    }
                    
                    // 로딩 인디케이터
                    Box(
                        modifier = Modifier
                            .fillMaxWidth()
                            .padding(vertical = 16.dp),
                        contentAlignment = Alignment.Center
                    ) {
                        CircularProgressIndicator(
                            modifier = Modifier.size(24.dp),
                            color = Primary500,
                            strokeWidth = 2.dp
                        )
                    }
                } else {
                    // 마지막 페이지인 경우 메시지 표시
                    Box(
                        modifier = Modifier
                            .fillMaxWidth()
                            .padding(vertical = 16.dp),
                        contentAlignment = Alignment.Center
                    ) {
                        Text(
                            text = "더 이상 거래 내역이 없습니다.",
                            fontSize = 14.sp,
                            color = Color.Gray,
                            fontFamily = SCDreamFontFamily
                        )
                    }
                }
            } else {
                // 아이템 사이 간격
                Spacer(modifier = Modifier.height(12.dp))
            }
        }
    }
}

@Composable
fun TradeHistoryItem(history: TradeHistory) {
    val isSell = history.tradeType == "SELL"
    val tradedDate = history.tradedAt.substringBefore("T").replace("-", ".")
    val tradedTime = history.tradedAt.substringAfter("T").take(8)
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .padding(vertical = 8.dp),
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.SpaceBetween
    ) {
        if (isSell) {  // SELL 타입일 때 (매도)
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
                        .height(50.dp),
                    verticalAlignment = Alignment.CenterVertically
                ) {
                    // 종목명만 표시
                    Row(
                        modifier = Modifier
                            .weight(1f)
                            .background(Color(0xFFF5F5F5))
                            .height(50.dp)
                            .padding(horizontal = 16.dp),
                        verticalAlignment = Alignment.CenterVertically
                    ) {
                        Text(
                            text = history.stockName,
                            fontSize = 10.sp,
                            fontWeight = FontWeight.W700,
                            fontFamily = SCDreamFontFamily
                        )
                    }
                    // 빨간색 부분 (매도가 스타일)
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
                                text = "매도가 ${history.unitPrice.toFormattedString()}원",
                                fontSize = 8.sp,
                                color = Color.White,
                                fontFamily = SCDreamFontFamily,
                                fontWeight = FontWeight.Bold
                            )
                            Text(
                                text = "${history.quantity}주",
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
                    text = tradedDate,
                    fontSize = 10.sp,
                    color = Color.Gray,
                    fontFamily = SCDreamFontFamily
                )
                Text(
                    text = tradedTime,
                    fontSize = 10.sp,
                    color = Color.Gray,
                    fontFamily = SCDreamFontFamily
                )
            }
        } else {  // BUY 타입일 때 (매수)
            // 왼쪽: 날짜 시간
            Column(
                horizontalAlignment = Alignment.Start,
                modifier = Modifier.width(70.dp)
            ) {
                Text(
                    text = tradedDate,
                    fontSize = 10.sp,
                    color = Color.Gray,
                    fontFamily = SCDreamFontFamily
                )
                Text(
                    text = tradedTime,
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
                        .height(50.dp),
                    verticalAlignment = Alignment.CenterVertically
                ) {
                    // 파란색 부분 (매수가 스타일)
                    Box(
                        modifier = Modifier
                            .background(Color(0xFF4DABF7))
                            .height(50.dp)
                            .padding(horizontal = 16.dp)
                            .clip(RoundedCornerShape(topStart = 16.dp, bottomStart = 16.dp)),
                        contentAlignment = Alignment.Center
                    ) {
                        Column(
                            horizontalAlignment = Alignment.CenterHorizontally
                        ) {
                            Text(
                                text = "매수가 ${history.unitPrice.toFormattedString()}원",
                                fontSize = 8.sp,
                                color = Color.White,
                                fontFamily = SCDreamFontFamily,
                                fontWeight = FontWeight.Bold
                            )
                            Text(
                                text = "${history.quantity}주",
                                fontSize = 8.sp,
                                color = Color.White,
                                fontFamily = SCDreamFontFamily,
                                fontWeight = FontWeight.Bold
                            )
                        }
                    }
                    // 종목명만 표시
                    Row(
                        modifier = Modifier
                            .weight(1f)
                            .background(Color(0xFFF5F5F5))
                            .height(50.dp)
                            .padding(horizontal = 16.dp),
                        verticalAlignment = Alignment.CenterVertically,
                        horizontalArrangement = Arrangement.End
                    ) {
                        Text(
                            text = history.stockName,
                            fontSize = 10.sp,
                            fontWeight = FontWeight.W700,
                            fontFamily = SCDreamFontFamily
                        )
                    }
                }
            }
        }
    }
}

// 관심 종목 탭 내용
@Composable
fun FavoriteStocksTabContent(
    navController: NavController,
    mainViewModel: MainViewModel,
    authViewModel: AuthViewModel
) {
    val context = LocalContext.current
    val watchlistItems = mainViewModel.watchlistItems.collectAsState().value
    val isLoading by mainViewModel.isLoading.collectAsState()
    val error by mainViewModel.error.collectAsState()
    
    // 컴포넌트가 처음 로드될 때 관심 종목 데이터를 불러옴
    LaunchedEffect(Unit) {
        // 서비스 초기화 먼저 실행
        mainViewModel.initializeServices(context)
        // 그 다음 관심 종목 로드
        mainViewModel.loadWatchlist(context)
    }
    
    // 로딩 상태 표시
    if (isLoading && watchlistItems.isEmpty()) {
        Box(modifier = Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
            CircularProgressIndicator(color = Primary500)
        }
        return
    }
    
    // 에러 상태 표시
    if (error != null && watchlistItems.isEmpty()) {
        Box(modifier = Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
            Text(text = "데이터를 불러오는데 실패했습니다.", color = Color.Gray)
        }
        return
    }
    
    // 데이터가 없는 경우 표시
    if (watchlistItems.isEmpty()) {
        Box(modifier = Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
            Text(text = "관심 종목이 없습니다.", color = Color.Gray, fontFamily = SCDreamFontFamily)
        }
        return
    }
    
    // 관심 종목 목록 표시
    LazyColumn(
        modifier = Modifier
            .fillMaxWidth()
            .padding(horizontal = 36.dp),
        contentPadding = PaddingValues(vertical = 16.dp)
    ) {
        items(watchlistItems.size) { index ->
            // HomeScreen의 FavoriteStockItem과 유사한 방식으로 구현
            Surface(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(vertical = 8.dp)
                    .clickable {
                        navController.navigate(Screen.StockDetail.createRoute(
                            stockCode = watchlistItems[index].stockCode,
                            currentPrice = watchlistItems[index].currentPrice.toString(),
                            fluctuationRate = watchlistItems[index].changeRate
                        )) {
                            launchSingleTop = true
                            restoreState = true
                        }
                    },
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
                    // 왼쪽: 로고와 정보
                    Row(
                        modifier = Modifier.weight(1f),
                        verticalAlignment = Alignment.CenterVertically
                    ) {
                        AsyncImage(
                            model = "https://thumb.tossinvest.com/image/resized/96x0/https%3A%2F%2Fstatic.toss.im%2Fpng-icons%2Fsecurities%2Ficn-sec-fill-${watchlistItems[index].stockCode}.png",
                            contentDescription = "주식 로고",
                            modifier = Modifier
                                .size(40.dp)
                                .clip(CircleShape)
                                .background(Color.White)
                        )
                        Spacer(modifier = Modifier.width(12.dp))
                        Column {
                            Text(
                                text = watchlistItems[index].stockName,
                                fontSize = 13.sp,
                                fontWeight = FontWeight.W700,
                                fontFamily = SCDreamFontFamily
                            )
                            Row(
                                verticalAlignment = Alignment.CenterVertically
                            ) {
                                Text(
                                    text = "${watchlistItems[index].currentPrice.toFormattedString()}원",
                                    fontSize = 11.sp,
                                    fontWeight = FontWeight.W500,
                                    fontFamily = SCDreamFontFamily
                                )
                                Text(
                                    text = " ${watchlistItems[index].changeRate}%",
                                    color = when {
                                        watchlistItems[index].changeRate.startsWith("+") -> Color.Red
                                        watchlistItems[index].changeRate.startsWith("-") -> Color.Blue
                                        else -> Color.Gray
                                    },
                                    fontSize = 11.sp,
                                    fontWeight = FontWeight.W500,
                                    fontFamily = SCDreamFontFamily
                                )
                            }
                        }
                    }
                    
                    // 오른쪽: 하트 아이콘
                    IconButton(
                        onClick = {
                            if (authViewModel.isLoggedIn.value) {
                                mainViewModel.removeFromWatchlist(
                                    stockCode = watchlistItems[index].stockCode,
                                    context = context,
                                    onSuccess = { mainViewModel.loadWatchlist(context) }
                                )
                            }
                        },
                        modifier = Modifier.size(22.dp)
                    ) {
                        Icon(
                            imageVector = Icons.Filled.Favorite,
                            contentDescription = "관심 종목 삭제",
                            tint = Color(0xFFFF4081),
                            modifier = Modifier.size(22.dp)
                        )
                    }
                }
            }
            
            if (index < watchlistItems.size - 1) {
                Spacer(modifier = Modifier.height(8.dp))
            }
        }
    }
}

// 거래 타입 (매수/매도)
enum class TradeType { BUY, SELL } 