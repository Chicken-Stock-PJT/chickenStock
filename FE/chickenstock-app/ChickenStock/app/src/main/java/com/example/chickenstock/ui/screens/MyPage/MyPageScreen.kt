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
import androidx.compose.material.icons.filled.Refresh
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
import androidx.navigation.NavHostController
import androidx.navigation.NavBackStackEntry
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
import androidx.compose.material.icons.filled.Delete
import com.google.accompanist.swiperefresh.SwipeRefresh
import com.google.accompanist.swiperefresh.rememberSwipeRefreshState
import androidx.compose.animation.core.Animatable
import androidx.compose.animation.core.tween
import androidx.compose.ui.graphics.graphicsLayer

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
    navController: NavHostController,
    navBackStackEntry: NavBackStackEntry,
    authViewModel: AuthViewModel = viewModel(factory = AuthViewModel.Factory(LocalContext.current)),
    mainViewModel: MainViewModel = viewModel()
) {
    // 쿼리 파라미터 읽기
    val initialTab = navBackStackEntry.arguments?.getString("tab")
    val initialPending = navBackStackEntry.arguments?.getString("pending") == "true"

    // 초기 탭 인덱스 설정
    var selectedTabIndex by remember(initialTab) { 
        mutableStateOf(
            when (initialTab) {
                "portfolio" -> 0
                "history", "trade" -> 1
                "favorite" -> 2
                null -> 0
                else -> 0
            }
        )
    }

    val tabs = listOf("포트폴리오", "거래 기록", "관심 종목")
    
    val context = LocalContext.current
    val tokenManager = remember { com.example.chickenstock.data.TokenManager.getInstance(context) }

    val dashboardData by mainViewModel.dashboardData.collectAsState()
    val isLoading by mainViewModel.isLoading.collectAsState()
    val error by mainViewModel.error.collectAsState()

    var isRefreshing by remember { mutableStateOf(false) }
    var selectedTradeTab by remember { mutableStateOf("체결 내역") }

    val rotation = remember { Animatable(0f) }
    val scope = rememberCoroutineScope()

    // 포트폴리오 탭 진입 시마다 새로고침
    LaunchedEffect(selectedTabIndex) {
        if (selectedTabIndex == 0) {
            val memberService = try {
                val field = mainViewModel.javaClass.getDeclaredField("memberService")
                field.isAccessible = true
                field.get(mainViewModel) as? com.example.chickenstock.api.MemberService
            } catch (e: Exception) { null }
            if (memberService != null) {
                mainViewModel.loadDashboard(memberService, context)
            } else {
                mainViewModel.initializeServices(context)
                val ms = try {
                    val field = mainViewModel.javaClass.getDeclaredField("memberService")
                    field.isAccessible = true
                    field.get(mainViewModel) as? com.example.chickenstock.api.MemberService
                } catch (e: Exception) { null }
                if (ms != null) {
                    mainViewModel.loadDashboard(ms, context)
                }
            }
        }
    }

    Scaffold(
        containerColor = Color(0xFFF5F5F5)
    ) { innerPadding ->
        Column(
            modifier = Modifier
                .fillMaxSize()
        ) {
            // 프로필 섹션 (고정)
            Box(
                modifier = Modifier
                    .fillMaxWidth()
                    .background(
                        color = Color(0xFFFFFBEB),
                        shape = RoundedCornerShape(topStart = 24.dp, topEnd = 24.dp)
                    )
                    .padding(horizontal = 16.dp, vertical = 8.dp)
            ) {
                // 새로고침 아이콘 (왼쪽 상단)
                IconButton(
                    onClick = {
                        if (!isRefreshing) {
                            isRefreshing = true
                            scope.launch {
                                rotation.snapTo(0f)
                                rotation.animateTo(
                                    targetValue = 360f,
                                    animationSpec = tween(durationMillis = 700)
                                )
                                // 새로고침 로직 실행 + 로그
                                when (selectedTabIndex) {
                                    0 -> {
                                        Log.d("MyPageScreen", "[새로고침] 포트폴리오 탭: 자산/보유주식 새로고침")
                                        val memberService = try {
                                            val field = mainViewModel.javaClass.getDeclaredField("memberService")
                                            field.isAccessible = true
                                            field.get(mainViewModel) as? com.example.chickenstock.api.MemberService
                                        } catch (e: Exception) { null }
                                        if (memberService != null) mainViewModel.loadDashboard(memberService, context)
                                    }
                                    1 -> {
                                        if (selectedTradeTab == "체결 내역") {
                                            Log.d("MyPageScreen", "[새로고침] 거래기록 탭: 체결 내역 새로고침")
                                            mainViewModel.loadTradeHistories(context, 10)
                                        } else {
                                            Log.d("MyPageScreen", "[새로고침] 거래기록 탭: 미체결 내역 새로고침")
                                            val token = tokenManager.getAccessToken()
                                            if (!token.isNullOrBlank()) mainViewModel.loadPendingOrders("Bearer $token")
                                        }
                                    }
                                    2 -> {
                                        Log.d("MyPageScreen", "[새로고침] 관심 종목 탭: 관심종목 새로고침")
                                        mainViewModel.loadWatchlist(context)
                                    }
                                }
                                isRefreshing = false
                            }
                        }
                    },
                    enabled = !isRefreshing,
                    modifier = Modifier.align(Alignment.TopStart)
                ) {
                    Icon(
                        imageVector = Icons.Filled.Refresh,
                        contentDescription = "새로고침",
                        tint = if (isRefreshing) Color.Gray else Color.Black,
                        modifier = Modifier.graphicsLayer { rotationZ = rotation.value }
                    )
                }
                
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
                                .fillMaxWidth(),
                            contentAlignment = Alignment.Center
                        ) {
                            AsyncImage(
                                model = R.drawable.logo, // res/drawable/logo.png 등 앱 로고 리소스 사용
                                contentDescription = "앱 로고",
                                modifier = Modifier
                                    .size(64.dp)
                                    .clip(CircleShape)
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
                    0 -> {
                        SwipeRefresh(
                            state = rememberSwipeRefreshState(isRefreshing),
                            onRefresh = {
                                isRefreshing = true
                                android.util.Log.d("MyPageScreen", "포트폴리오 새로고침 시도")
                                // memberService를 안전하게 꺼내서 넘김
                                val memberService = try {
                                    val field = mainViewModel.javaClass.getDeclaredField("memberService")
                                    field.isAccessible = true
                                    field.get(mainViewModel) as? com.example.chickenstock.api.MemberService
                                } catch (e: Exception) {
                                    null
                                }
                                if (memberService != null) {
                                    mainViewModel.loadDashboard(memberService, context)
                                } else {
                                    mainViewModel.initializeServices(context)
                                    val ms = try {
                                        val field = mainViewModel.javaClass.getDeclaredField("memberService")
                                        field.isAccessible = true
                                        field.get(mainViewModel) as? com.example.chickenstock.api.MemberService
                                    } catch (e: Exception) {
                                        null
                                    }
                                    if (ms != null) {
                                        mainViewModel.loadDashboard(ms, context)
                                    }
                                }
                                isRefreshing = false
                            }
                        ) {
                            DashboardPortfolioTabContent(dashboardData = dashboardData, navController = navController)
                        }
                    }
                    1 -> TradeHistoryTabContent(mainViewModel = mainViewModel, initialPending = initialPending)
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
                .weight(1f)
                .background(if (isSelected) Secondary50 else Color.Transparent),
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
fun PortfolioItem(holding: com.example.chickenstock.model.DashboardHolding, totalValuation: Int, navController: NavHostController) {
    Card(
        modifier = Modifier
            .fillMaxWidth()
            .padding(horizontal = 32.dp, vertical = 12.dp)
            .clickable {
                navController.navigate(
                    com.example.chickenstock.navigation.Screen.StockDetail.createRoute(
                        stockCode = holding.stockCode,
                        currentPrice = holding.currentPrice.toString(),
                        fluctuationRate = holding.returnRate.toString()
                    )
                )
            },
        shape = RoundedCornerShape(12.dp),
        colors = CardDefaults.cardColors(
            containerColor = Color.White
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
                        model = "https://thumb.tossinvest.com/image/resized/96x0/https%3A%2F%2Fstatic.toss.im%2Fpng-icons%2Fsecurities%2Ficn-sec-fill-${holding.stockCode}.png",
                        contentDescription = "주식 로고",
                        modifier = Modifier
                            .size(40.dp)
                            .clip(CircleShape)
                            .background(Color.White),
                        error = painterResource(id = R.drawable.logo)
                    )

                    Spacer(modifier = Modifier.width(12.dp))

                    // 주식 이름
                    Text(
                        text = holding.stockName,
                        fontSize = 16.sp,
                        fontWeight = FontWeight.Bold,
                        fontFamily = SCDreamFontFamily
                    )
                }

                // 보유 수량
                Text(
                    text = "${holding.quantity}주",
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
                    text = "평균 매수 금액: ${holding.averagePrice.toFormattedString()} 원",
                    fontSize = 14.sp,
                    color = Color.Gray,
                    fontFamily = SCDreamFontFamily
                )

                Spacer(modifier = Modifier.height(4.dp))

                Text(
                    text = "현재 주식 가치: ${holding.currentPrice.toFormattedString()} 원",
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
                    text = "${holding.valuationAmount.toFormattedString()} 원",
                    fontSize = 16.sp,
                    fontWeight = FontWeight.Bold,
                    fontFamily = SCDreamFontFamily
                )
                // 손익 및 수익률
                val profitLoss = holding.profitLoss
                val returnRate = holding.returnRate
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
                    fontSize = 12.sp,
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
fun DashboardPortfolioTabContent(
    dashboardData: com.example.chickenstock.model.DashboardResponse?,
    navController: NavHostController
) {
    if (dashboardData == null) {
        Box(modifier = Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
            Text(text = "데이터가 없습니다.", color = Color.Gray)
        }
        return
    }
    val totalAsset = dashboardData.totalAsset
    val memberMoney = dashboardData.memberMoney
    val stockValuation = dashboardData.stockValuation
    val profit = dashboardData.totalProfitLoss
    val returnRate = dashboardData.totalReturnRate
    val holdings = dashboardData.holdings

    // 그래프 데이터 준비
    val totalStockValuation = holdings.sumOf { it.valuationAmount }
    val stockRatios = if (holdings.isNotEmpty() && totalStockValuation > 0) {
        holdings.map {
            val calculatedRatio = it.valuationAmount.toDouble() / totalStockValuation.toDouble()
            if (calculatedRatio.isNaN() || calculatedRatio < 0.0) 0.0 else calculatedRatio
        }
    } else emptyList()
    val ratios = if (stockRatios.isEmpty()) listOf(1.0) else stockRatios
    val donutColors = listOf(Color(0xFF1428A0), Color(0xFFFF8A00), Color(0xFF4DABF7), Color(0xFFFF6B6B), Color(0xFF8BC34A), Color(0xFFBDBDBD))
    val legends = if (holdings.isNotEmpty()) holdings.map { it.stockName } else listOf("-")
    val safeLegendsSize = minOf(legends.size, ratios.size)
    val sortedIndices = (0 until safeLegendsSize).sortedByDescending { ratios[it] }

    LazyColumn(
        modifier = Modifier.fillMaxWidth()
    ) {
        item(key = "header") {
            Spacer(modifier = Modifier.height(16.dp))
            Card(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(horizontal = 16.dp),
                colors = CardDefaults.cardColors(containerColor = Color.White),
                shape = RoundedCornerShape(16.dp),
                elevation = CardDefaults.cardElevation(defaultElevation = 0.dp)
            ) {
                Column(
                    modifier = Modifier
                        .fillMaxWidth()
                        .padding(horizontal = 24.dp, vertical = 20.dp)
                ) {
                    Row(
                        modifier = Modifier.fillMaxWidth(),
                        verticalAlignment = Alignment.Bottom
                    ) {
                        Column(modifier = Modifier.weight(1f)) {
                            Text(
                                text = "총 자산",
                                fontSize = 16.sp,
                                fontWeight = FontWeight.Bold,
                                color = Color.Black
                            )
                            Spacer(modifier = Modifier.height(2.dp))
                            Row(verticalAlignment = Alignment.Bottom) {
                                Text(
                                    text = totalAsset.toFormattedString(),
                                    fontSize = 28.sp,
                                    fontWeight = FontWeight.Bold,
                                    color = Color.Black
                                )
                                Text(
                                    text = "원",
                                    fontSize = 18.sp,
                                    fontWeight = FontWeight.W500,
                                    color = Color.Black,
                                    modifier = Modifier.padding(start = 2.dp, bottom = 2.dp)
                                )
                            }
                        }
                        val returnRateText = when {
                            returnRate > 0.0 -> "+${String.format("%.2f", returnRate)}%"
                            returnRate < 0.0 -> "${String.format("%.2f", returnRate)}%"
                            else -> "0.00%"
                        }
                        val returnRateColor = when {
                            returnRate > 0.0 -> Color(0xFFE74C3C)
                            returnRate < 0.0 -> Color(0xFF1976D2)
                            else -> Color.Gray
                        }
                        Text(
                            text = returnRateText,
                            fontSize = 20.sp,
                            fontWeight = FontWeight.Bold,
                            color = returnRateColor,
                            modifier = Modifier.padding(start = 8.dp, bottom = 2.dp)
                        )
                    }
                    Spacer(modifier = Modifier.height(12.dp))
                    Text(
                        text = "보유 현금: ${memberMoney.toFormattedString()}원",
                        fontSize = 15.sp,
                        color = Color.Black,
                        fontWeight = FontWeight.W600
                    )
                    Spacer(modifier = Modifier.height(2.dp))
                    Text(
                        text = "평가 금액: ${stockValuation.toFormattedString()}원",
                        fontSize = 15.sp,
                        color = Color.Black,
                        fontWeight = FontWeight.W600
                    )
                }
            }
        }
        item(key = "today-profit-cards") {
            Spacer(modifier = Modifier.height(8.dp))
            Row(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(horizontal = 24.dp),
                horizontalArrangement = Arrangement.spacedBy(12.dp)
            ) {
                // 금일 실현 손익 카드
                Card(
                    modifier = Modifier
                        .weight(1f)
                        .height(70.dp),
                    shape = RoundedCornerShape(12.dp),
                    colors = CardDefaults.cardColors(containerColor = Color.White),
                    elevation = CardDefaults.cardElevation(defaultElevation = 1.dp)
                ) {
                    Column(
                        modifier = Modifier.fillMaxSize(),
                        verticalArrangement = Arrangement.Center,
                        horizontalAlignment = Alignment.CenterHorizontally
                    ) {
                        Text(
                            text = "금일 손익",
                            fontSize = 13.sp,
                            color = Color.Gray,
                            fontWeight = FontWeight.W600,
                            fontFamily = SCDreamFontFamily
                        )
                        Spacer(modifier = Modifier.height(2.dp))
                        Text(
                            text = "${if (dashboardData.todayProfitLoss < 0) "-" else ""}${kotlin.math.abs(dashboardData.todayProfitLoss).toFormattedString()}원",
                            fontSize = 16.sp,
                            fontWeight = FontWeight.Bold,
                            color = when {
                                dashboardData.todayProfitLoss > 0 -> Color.Red
                                dashboardData.todayProfitLoss < 0 -> Color.Blue
                                else -> Color.Gray
                            },
                            fontFamily = SCDreamFontFamily
                        )
                    }
                }
                // 금일 수익률 카드
                Card(
                    modifier = Modifier
                        .weight(1f)
                        .height(70.dp),
                    shape = RoundedCornerShape(12.dp),
                    colors = CardDefaults.cardColors(containerColor = Color.White),
                    elevation = CardDefaults.cardElevation(defaultElevation = 1.dp)
                ) {
                    Column(
                        modifier = Modifier.fillMaxSize(),
                        verticalArrangement = Arrangement.Center,
                        horizontalAlignment = Alignment.CenterHorizontally
                    ) {
                        Text(
                            text = "금일 수익률",
                            fontSize = 13.sp,
                            color = Color.Gray,
                            fontWeight = FontWeight.W600,
                            fontFamily = SCDreamFontFamily
                        )
                        Spacer(modifier = Modifier.height(2.dp))
                        Text(
                            text = "${if (dashboardData.todayReturnRate < 0) "-" else ""}${String.format("%.2f", kotlin.math.abs(dashboardData.todayReturnRate))}%",
                            fontSize = 16.sp,
                            fontWeight = FontWeight.Bold,
                            color = when {
                                dashboardData.todayReturnRate > 0 -> Color.Red
                                dashboardData.todayReturnRate < 0 -> Color.Blue
                                else -> Color.Gray
                            },
                            fontFamily = SCDreamFontFamily
                        )
                    }
                }
            }
            // 업데이트 시간 표시
            Box(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(end = 24.dp, top = 2.dp),
                contentAlignment = Alignment.CenterEnd
            ) {
                if (!dashboardData.updatedAt.isNullOrBlank()) {
                    Text(
                        text = "업데이트: ${dashboardData.updatedAt}",
                        fontSize = 11.sp,
                        color = Color.Gray,
                        fontFamily = SCDreamFontFamily
                    )
                }
            }
        }
        item{Spacer(modifier = Modifier.height(8.dp))}
        item(key = "chart") {
            if (ratios.isNotEmpty()) {
                Column(
                    modifier = Modifier
                        .fillMaxWidth()
                        .padding(top = 8.dp, bottom = 8.dp),
                    horizontalAlignment = Alignment.CenterHorizontally
                ) {
                    AnimatedDonutChart(
                        ratios = ratios,
                        colors = donutColors,
                        modifier = Modifier.align(Alignment.CenterHorizontally),
                        size = 160.dp
                    )
                    Spacer(modifier = Modifier.height(16.dp))
                    Box(
                        modifier = Modifier
                            .fillMaxWidth()
                            .padding(horizontal = 8.dp)
                    ) {
                        Column(modifier = Modifier.fillMaxWidth()) {
                            androidx.compose.foundation.lazy.LazyRow(
                                modifier = Modifier.fillMaxWidth(),
                                contentPadding = PaddingValues(horizontal = 8.dp),
                                horizontalArrangement = if (safeLegendsSize <= 2)
                                    Arrangement.spacedBy(16.dp, Alignment.CenterHorizontally)
                                else
                                    Arrangement.spacedBy(16.dp)
                            ) {
                                items(sortedIndices.size) { i ->
                                    val index = sortedIndices[i]
                                    LegendItem(
                                        color = donutColors[index % donutColors.size],
                                        name = legends[index],
                                        ratio = ratios[index]
                                    )
                                }
                            }
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
        items(
            items = holdings,
            key = { holding -> holding.stockCode }
        ) { holding ->
            PortfolioItem(holding = holding, totalValuation = totalStockValuation, navController = navController)
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
            containerColor = Color.White
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
    mainViewModel: MainViewModel,
    initialPending: Boolean = false
) {
    val context = LocalContext.current
    val tradeHistories by mainViewModel.tradeHistoryList.collectAsState()
    val isLoading by mainViewModel.isLoading.collectAsState()
    val error by mainViewModel.error.collectAsState()
    val hasNext by mainViewModel.hasNext.collectAsState()
    val nextCursor by mainViewModel.nextCursor.collectAsState()

    // 어떤 내역 탭이 선택됐는지 상태
    var selectedTab by remember { mutableStateOf(if (initialPending) "미체결 내역" else "체결 내역") }

    // 컴포넌트가 처음 로드될 때 거래 내역 데이터를 불러옴
    LaunchedEffect(Unit) {
        // 서비스 초기화 먼저 실행
        mainViewModel.initializeServices(context)
        mainViewModel.initializeTradeHistoryService(context)
        // 그 다음 거래 내역 로드
        mainViewModel.loadTradeHistories(context, 10)
    }

    // 상단 버튼 Row
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .padding(horizontal = 16.dp, vertical = 8.dp),
        horizontalArrangement = Arrangement.spacedBy(12.dp)
    ) {
        Button(
            onClick = { selectedTab = "체결 내역" },
            colors = ButtonDefaults.buttonColors(
                containerColor = if (selectedTab == "체결 내역") Primary500 else Color.White,
                contentColor = if (selectedTab == "체결 내역") Color.Black else Color.Gray
            ),
            shape = RoundedCornerShape(12.dp),
            modifier = Modifier.weight(1f)
        ) {
            Text("체결 내역", fontWeight = FontWeight.Bold, fontFamily = SCDreamFontFamily)
        }
        Button(
            onClick = { selectedTab = "미체결 내역" },
            colors = ButtonDefaults.buttonColors(
                containerColor = if (selectedTab == "미체결 내역") Primary500 else Color.White,
                contentColor = if (selectedTab == "미체결 내역") Color.Black else Color.Gray
            ),
            shape = RoundedCornerShape(12.dp),
            modifier = Modifier.weight(1f)
        ) {
            Text("미체결 내역", fontWeight = FontWeight.Bold, fontFamily = SCDreamFontFamily)
        }
    }

    // 아래 영역: 선택된 탭에 따라 내용 분기
    if (selectedTab == "체결 내역") {
        // 기존 체결 내역 UI
        if (isLoading && tradeHistories.isEmpty()) {
            Box(modifier = Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
                CircularProgressIndicator(color = Primary500)
            }
            return
        }
        if (error != null && tradeHistories.isEmpty()) {
            Box(modifier = Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
                Text(text = "데이터를 불러오는데 실패했습니다.", color = Color.Gray)
            }
            return
        }
        if (tradeHistories.isEmpty()) {
            Box(modifier = Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
                Text(text = "거래 내역이 없습니다.", color = Color.Gray, fontFamily = SCDreamFontFamily)
            }
            return
        }
        LazyColumn(
            modifier = Modifier.fillMaxWidth(),
            contentPadding = PaddingValues(16.dp)
        ) {
            items(tradeHistories.size) { index ->
                TradeHistoryItem(history = tradeHistories[index])
                if (index == tradeHistories.size - 1 && hasNext) {
                        LaunchedEffect(nextCursor) {
                            mainViewModel.loadTradeHistories(context, 10, nextCursor)
                        }
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
                } else if (index != tradeHistories.size - 1) {
                    Spacer(modifier = Modifier.height(12.dp))
                }
            }
        }
    } else {
        // 미체결 내역 영역
        val pendingOrders by mainViewModel.pendingOrders.collectAsState()
        val context = LocalContext.current
        val token = remember { com.example.chickenstock.data.TokenManager.getInstance(context).getAccessToken() }
        // 최초 진입 시 데이터 로드
        LaunchedEffect(Unit) {
            if (!token.isNullOrBlank()) {
                mainViewModel.loadPendingOrders("Bearer $token")
            }
        }
        if (pendingOrders.isEmpty()) {
            Box(
                modifier = Modifier.fillMaxSize(),
                contentAlignment = Alignment.Center
            ) {
                Text("미체결 내역이 없습니다.", color = Color.Gray, fontFamily = SCDreamFontFamily)
            }
        } else {
            var showCancelDialog by remember { mutableStateOf(false) }
            var cancelOrderId by remember { mutableStateOf<Int?>(null) }
            var isCancelLoading by remember { mutableStateOf(false) }
            var cancelErrorMessage by remember { mutableStateOf<String?>(null) }
            LazyColumn(
                modifier = Modifier.fillMaxWidth(),
                contentPadding = PaddingValues(16.dp)
            ) {
                items(pendingOrders) { order ->
                    Card(
                        modifier = Modifier
                            .fillMaxWidth()
                            .padding(vertical = 6.dp),
                        shape = RoundedCornerShape(12.dp),
                        colors = CardDefaults.cardColors(containerColor = Color.White),
                        elevation = CardDefaults.cardElevation(defaultElevation = 1.dp)
                    ) {
                        Row(
                            modifier = Modifier
                                .fillMaxWidth()
                                .padding(16.dp),
                            verticalAlignment = Alignment.CenterVertically,
                            horizontalArrangement = Arrangement.SpaceBetween
                        ) {
                            Row(verticalAlignment = Alignment.CenterVertically) {
                                AsyncImage(
                                    model = "https://thumb.tossinvest.com/image/resized/96x0/https%3A%2F%2Fstatic.toss.im%2Fpng-icons%2Fsecurities%2Ficn-sec-fill-${order.stockCode}.png",
                                    contentDescription = "주식 로고",
                                    modifier = Modifier
                                        .size(40.dp)
                                        .clip(CircleShape)
                                        .background(Color.White),
                                    error = painterResource(id = R.drawable.logo)
                                )
                                Spacer(modifier = Modifier.width(12.dp))
                                Column {
                                    Text(
                                        text = order.stockName,
                                        fontSize = 15.sp,
                                        fontWeight = FontWeight.Bold,
                                        fontFamily = SCDreamFontFamily
                                    )
                                    Row(verticalAlignment = Alignment.CenterVertically) {
                                        Text(
                                            text = if (order.orderType == "BUY") "매수" else "매도",
                                            fontSize = 13.sp,
                                            color = if (order.orderType == "BUY") Color(0xFF1976D2) else Color(0xFFE74C3C),
                                            fontWeight = FontWeight.W600,
                                            fontFamily = SCDreamFontFamily
                                        )
                                        Spacer(modifier = Modifier.width(8.dp))
                                        Text(
                                            text = "수량: ${order.quantity}",
                                            fontSize = 13.sp,
                                            color = Color.Gray,
                                            fontFamily = SCDreamFontFamily
                                        )
                                        Spacer(modifier = Modifier.width(8.dp))
                                        Text(
                                            text = "목표가: ${order.targetPrice.toString()}원",
                                            fontSize = 13.sp,
                                            color = Color.Gray,
                                            fontFamily = SCDreamFontFamily
                                        )
                                    }
                                    Text(
                                        text = "주문 시간: ${order.createdAt.replace("T", " ")}",
                                        fontSize = 12.sp,
                                        color = Color.Gray,
                                        fontFamily = SCDreamFontFamily
                                    )
                                }
                            }
                            IconButton(onClick = {
                                cancelOrderId = order.orderId
                                cancelErrorMessage = null
                                showCancelDialog = true
                            }) {
                                Icon(
                                    imageVector = Icons.Filled.Delete,
                                    contentDescription = "주문 삭제",
                                    tint = Color.Gray
                                )
                            }
                        }
                    }
                }
            }
            if (showCancelDialog) {
                AlertDialog(
                    onDismissRequest = { if (!isCancelLoading) showCancelDialog = false },
                    title = { Text("주문 취소", fontFamily = SCDreamFontFamily, color = Color.Black, fontWeight = FontWeight.Bold) },
                    text = {
                        if (isCancelLoading) {
                            Box(Modifier.fillMaxWidth(), contentAlignment = Alignment.Center) {
                                CircularProgressIndicator(modifier = Modifier.size(24.dp), color = Color.Red, strokeWidth = 2.dp)
                            }
                        } else if (cancelErrorMessage != null) {
                            Text(cancelErrorMessage!!, color = Color.Red, fontFamily = SCDreamFontFamily, textAlign = TextAlign.Center)
                        } else {
                            Text("정말 주문을 취소하시겠습니까?", fontFamily = SCDreamFontFamily, color = Color.Black, textAlign = TextAlign.Center)
                        }
                    },
                    confirmButton = {
                        TextButton(
                            onClick = {
                                if (cancelOrderId != null) {
                                    isCancelLoading = true
                                    cancelErrorMessage = null
                                    mainViewModel.cancelPendingOrder(
                                        context = context,
                                        orderId = cancelOrderId!!,
                                        onSuccess = {
                                            isCancelLoading = false
                                            showCancelDialog = false
                                        },
                                        onError = { msg ->
                                            isCancelLoading = false
                                            cancelErrorMessage = msg
                                        }
                                    )
                                }
                            },
                            enabled = !isCancelLoading
                        ) {
                            if (isCancelLoading) {
                                CircularProgressIndicator(modifier = Modifier.size(16.dp), color = Color.Red, strokeWidth = 2.dp)
                            } else {
                                Text("확인", color = Color.Red, fontFamily = SCDreamFontFamily, fontWeight = FontWeight.Bold)
                            }
                        }
                    },
                    dismissButton = {
                        TextButton(
                            onClick = { if (!isCancelLoading) showCancelDialog = false },
                            enabled = !isCancelLoading
                        ) {
                            Text("취소", color = Color(0xFF0066CC), fontFamily = SCDreamFontFamily, fontWeight = FontWeight.Bold)
                        }
                    },
                    containerColor = Color.White
                )
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
                            .background(Color.White)
                            .height(50.dp)
                            .padding(horizontal = 16.dp),
                        verticalAlignment = Alignment.CenterVertically
                    ) {
                        Text(
                            text = history.stockName,
                            fontSize = 10.sp,
                            fontWeight = FontWeight.W700,
                            fontFamily = SCDreamFontFamily,
                            color = Color.Black
                        )
                    }
                    // 빨간색 부분 (매도가 스타일)
                    Box(
                        modifier = Modifier
                            .background(Color(0xFF4DABF7))
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
                            .background(Color(0xFFFF6B6B))
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
                            .background(Color.White)
                            .height(50.dp)
                            .padding(horizontal = 16.dp),
                        verticalAlignment = Alignment.CenterVertically,
                        horizontalArrangement = Arrangement.End
                    ) {
                        Text(
                            text = history.stockName,
                            fontSize = 10.sp,
                            fontWeight = FontWeight.W700,
                            fontFamily = SCDreamFontFamily,
                            color = Color.Black
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
        contentPadding = PaddingValues(vertical = 4.dp)
    ) {
        items(watchlistItems.size) { index ->
            // HomeScreen의 FavoriteStockItem과 유사한 방식으로 구현
            Surface(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(vertical = 8.dp)
                    .clickable {
                        navController.navigate(Screen.StockDetail.createRoute(
                            stockCode = getPureStockCode(watchlistItems[index].stockCode),
                            currentPrice = watchlistItems[index].currentPrice.toString(),
                            fluctuationRate = watchlistItems[index].changeRate
                        ))
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
                            model = "https://thumb.tossinvest.com/image/resized/96x0/https%3A%2F%2Fstatic.toss.im%2Fpng-icons%2Fsecurities%2Ficn-sec-fill-${getPureStockCode(watchlistItems[index].stockCode)}.png",
                            contentDescription = "주식 로고",
                            modifier = Modifier
                                .size(40.dp)
                                .clip(CircleShape)
                                .background(Color.White),
                            error = painterResource(id = R.drawable.logo)
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
                                    stockCode = getPureStockCode(watchlistItems[index].stockCode),
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

fun getPureStockCode(stockCode: String): String {
    return stockCode.replace("_AL", "")
}