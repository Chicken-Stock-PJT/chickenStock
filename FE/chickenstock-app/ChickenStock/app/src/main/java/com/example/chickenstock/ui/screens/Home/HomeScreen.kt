package com.example.chickenstock.ui.screens.Home

import android.util.Log
import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Favorite
import androidx.compose.material.icons.filled.KeyboardArrowRight
import androidx.compose.material.icons.outlined.FavoriteBorder
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.navigation.NavHostController
import androidx.compose.ui.graphics.Shape
import com.example.chickenstock.R
import com.example.chickenstock.navigation.Screen
import com.example.chickenstock.ui.theme.*
import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.clickable
import coil.compose.AsyncImage
import com.example.chickenstock.viewmodel.MainViewModel
import com.example.chickenstock.ui.components.SegmentedControl
import com.example.chickenstock.ui.components.StockListItem
import com.example.chickenstock.ui.components.StockItem
import com.example.chickenstock.ui.components.MoreButton
import com.example.chickenstock.viewmodel.AuthViewModel
import com.example.chickenstock.api.RetrofitClient
import com.example.chickenstock.api.MemberService
import com.example.chickenstock.api.SimpleProfileResponse
import com.example.chickenstock.api.WatchlistItem
import com.example.chickenstock.model.PortfolioData
import com.example.chickenstock.model.Position
import com.example.chickenstock.model.StockUpdate
import com.example.chickenstock.model.TotalData
import androidx.compose.ui.platform.LocalContext
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.lazy.LazyListState
import androidx.compose.foundation.lazy.rememberLazyListState
import kotlinx.coroutines.launch
import com.example.chickenstock.api.StockService
import com.example.chickenstock.model.RankingItem
import kotlinx.coroutines.async
import kotlinx.coroutines.coroutineScope
import androidx.lifecycle.viewmodel.compose.viewModel
import androidx.compose.runtime.collectAsState
import com.google.accompanist.swiperefresh.SwipeRefresh
import com.google.accompanist.swiperefresh.rememberSwipeRefreshState
import com.example.chickenstock.data.TokenManager
import android.content.Context
import androidx.activity.compose.BackHandler
import android.app.Activity

// 임시 데이터 클래스
data class UserAssetInfo(
    val stockHoldings: List<StockHolding> = listOf(
        StockHolding("삼성전자", "005930", 54900, -3.1f),
        StockHolding("SK하이닉스", "000660", 174100, -3.5f)
    ),
    val favoriteStocks: List<StockHolding> = listOf(
        StockHolding("삼성전자", "005930", 54900, -3.1f),
        StockHolding("SK하이닉스", "000660", 174100, -3.5f),
        StockHolding("한화오션", "042660", 77100, -2.8f)
    )
)

data class StockHolding(
    val name: String,
    val code: String,
    val price: Int,
    val changeRate: Float,
    val totalPrice: Int = 3000000,  // 총 보유 금액
    val profitAmount: Int = 1000000,  // 수익 금액
    val profitRate: Float = 50.0f    // 수익률
)

// Int 확장 함수 추가
fun Int.toFormattedString(): String {
    return String.format("%,d", this)
}

// Long 확장 함수 추가
fun Long.toFormattedString(): String {
    return String.format("%,d", this)
}

// AssetInfoRow 컴포넌트 추가
@Composable
fun AssetInfoRow(label: String, value: String) {
    Row(
        modifier = Modifier.fillMaxWidth(),
        horizontalArrangement = Arrangement.SpaceBetween
    ) {
        Text(
            text = label,
            fontSize = 16.sp,
            fontWeight = FontWeight.W500,
            fontFamily = SCDreamFontFamily
        )
        Text(
            text = value,
            fontSize = 16.sp,
            fontWeight = FontWeight.W700,
            fontFamily = SCDreamFontFamily
        )
    }
}

// FavoriteStock 데이터 클래스 추가
data class FavoriteStock(
    val stockCode: String,
    val stockName: String,
    val currentPrice: String,
    val fluctuationRate: String
)

// FavoriteStockItem 컴포넌트 추가
@Composable
fun FavoriteStockItem(
    stock: FavoriteStock,
    navController: NavHostController,
    authViewModel: AuthViewModel,
    viewModel: MainViewModel,
    onRemove: () -> Unit
) {
    val isInWatchlist = viewModel.watchlist.value.contains(stock.stockCode)
    val scope = rememberCoroutineScope()
    val context = LocalContext.current
    
    Surface(
        modifier = Modifier
            .fillMaxWidth()
            .padding(vertical = 4.dp)
            .clickable {
                navController.navigate(Screen.StockDetail.createRoute(
                    stockCode = stock.stockCode,
                    currentPrice = stock.currentPrice,
                    fluctuationRate = stock.fluctuationRate
                )) {
                    launchSingleTop = true
                    restoreState = true
                }
            },
        color = Color.White,
        shadowElevation = 0.dp
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
                    model = "https://thumb.tossinvest.com/image/resized/96x0/https%3A%2F%2Fstatic.toss.im%2Fpng-icons%2Fsecurities%2Ficn-sec-fill-${stock.stockCode}.png",
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
                        text = stock.stockName,
                        fontSize = 13.sp,
                        fontWeight = FontWeight.W700,
                        fontFamily = SCDreamFontFamily
                    )
                    Row(
                        verticalAlignment = Alignment.CenterVertically
                    ) {
                        Text(
                            text = "${stock.currentPrice.replace("""[+\-]""".toRegex(), "").toIntOrNull()?.toFormattedString() ?: stock.currentPrice}원",
                            fontSize = 11.sp,
                            fontWeight = FontWeight.W500,
                            fontFamily = SCDreamFontFamily
                        )
                        Text(
                            text = " ${stock.fluctuationRate}%",
                            color = when {
                                stock.fluctuationRate.startsWith("+") -> Color.Red
                                stock.fluctuationRate.startsWith("-") -> Color.Blue
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
                        onRemove()
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
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun HomeScreen(
    navController: NavHostController,
    authViewModel: AuthViewModel,
    memberService: MemberService,
    stockService: StockService,
    viewModel: MainViewModel
) {
    val context = LocalContext.current
    val scope = rememberCoroutineScope()
    val dashboardData by viewModel.dashboardData.collectAsState()
    val userProfile by viewModel.userProfile.collectAsState()
    val error by viewModel.error.collectAsState()
    val isLoading by viewModel.isLoading.collectAsState()
    var isLoadingLocal by remember { mutableStateOf(false) }
    var isRefreshing by remember { mutableStateOf(false) }
    val portfolioData by viewModel.portfolioData.collectAsState()
    val pendingOrders by viewModel.pendingOrders.collectAsState()

    // 홈에서 뒤로가기 시 앱 종료 다이얼로그
    var showExitDialog by remember { mutableStateOf(false) }
    BackHandler {
        showExitDialog = true
    }
    if (showExitDialog) {
        AlertDialog(
            onDismissRequest = { showExitDialog = false },
            title = { Text("앱 종료", fontFamily = SCDreamFontFamily, color = Color.Black, fontWeight = FontWeight.Bold) },
            text = { Text("앱을 종료하시겠습니까?", fontFamily = SCDreamFontFamily, color = Color.Black) },
            confirmButton = {
                TextButton(onClick = {
                    showExitDialog = false
                    // 앱 종료
                    (context as? Activity)?.finishAffinity()
                }) {
                    Text("종료", color = Color.Red, fontFamily = SCDreamFontFamily, fontWeight = FontWeight.Bold)
                }
            },
            dismissButton = {
                TextButton(onClick = { showExitDialog = false }) {
                    Text("취소", color = Color(0xFF0066CC), fontFamily = SCDreamFontFamily, fontWeight = FontWeight.Bold)
                }
            },
            containerColor = Color(0xFFF5F5F5)
        )
    }

    // 화면이 처음 표시될 때만 초기화 작업 수행 (빈 리스트로 설정)
    DisposableEffect(key1 = Unit) {
        onDispose {
            // 화면이 사라질 때 코루틴 정리 (명시적으로 표현)
            scope.launch {
                // viewModel의 상태 정리가 필요한 경우 여기서 수행
            }
        }
    }

    // 진입 시 대시보드/닉네임 동시 로드
    LaunchedEffect(authViewModel.isLoggedIn.value) {
        val isLoggedIn = authViewModel.isLoggedIn.value
        if (isLoggedIn) {
            try {
                val memberService = RetrofitClient.getInstance(context).create(MemberService::class.java)
                viewModel.loadDashboard(memberService, context)
                viewModel.loadUserProfile(memberService, context) // 닉네임만 사용
                viewModel.loadWatchlist(context) // 홈 진입 시 관심종목도 함께 로드
            } catch (e: Exception) {
                Log.e("HomeScreen", "대시보드/프로필/관심종목 로드 오류: ${e.message}", e)
            }
        }
    }

    // 실시간 종목 랭킹
    var selectedRankingIndex by remember { mutableStateOf(0) }
    val rankingOptions = listOf("거래대금", "급상승", "급하락", "거래량")
    var stockRankings by remember { mutableStateOf<List<RankingItem>>(emptyList()) }
    var isLoadingRankings by remember { mutableStateOf(false) }
    var rankingError by remember { mutableStateOf<String?>(null) }
    
    // API 서비스 생성 - LaunchedEffect 안에서 사용할 변수
    val retrofit = remember { RetrofitClient.getInstance(context) }
    val stockService = remember { retrofit.create(StockService::class.java) }

    // API 엔드포인트 결정
    val currentEndpoint = remember(selectedRankingIndex) {
        when (selectedRankingIndex) {
            0 -> "stock/ranking/tradeAmount" // 거래대금
            1 -> "stock/ranking/fluctuationRate" // 급상승
            2 -> "stock/ranking/fluctuationRate" // 급하락
            3 -> "stock/ranking/volume" // 거래량
            else -> "stock/ranking/tradeAmount"
        }
    }

    // sortType 결정
    val currentSortType = remember(selectedRankingIndex) {
        when (selectedRankingIndex) {
            0 -> "1" // 거래대금
            1 -> "1" // 급상승 (상승률)
            2 -> "3" // 급하락 (하락률)
            3 -> "1" // 거래량 (기본값 1 사용)
            else -> "1"
        }
    }

    // 초기 데이터 로드
    LaunchedEffect(selectedRankingIndex) {
        // 현재 선택된 랭킹 인덱스 캡처
        val currentIndex = selectedRankingIndex
        val currentTypeName = rankingOptions[currentIndex]
        val currentSortTypeValue = currentSortType
        
        isLoadingRankings = true
        stockRankings = emptyList() // 초기화
        
        try {
            println("API 호출 시작")
            println("선택된 탭: $currentTypeName")
            println("마켓 타입: 000")
            println("정렬 타입: $currentSortTypeValue")
            
            coroutineScope {
                val response = if (currentIndex in listOf(1, 2)) {
                    // 급상승/급하락의 경우
                    println("급상승/급하락 API 호출")
                    stockService.getFluctuationRateRanking(
                        marketType = "000", // 전체
                        sortType = currentSortTypeValue
                    )
                } else if (currentIndex == 3) {
                    // 거래량의 경우
                    println("거래량 API 호출")
                    stockService.getVolumeRanking(
                        marketType = "000" // 전체
                    )
                } else {
                    // 거래대금의 경우
                    println("거래대금 API 호출")
                    stockService.getTradeAmountRanking(
                        marketType = "000" // 전체
                    )
                }

                if (response.isSuccessful) {
                    println("API 응답 성공")
                    response.body()?.let { rankingResponse ->
                        println("응답 데이터:")
                        println("- 아이템 개수: ${rankingResponse.rankingItems.size}")
                        
                        if (rankingResponse.rankingItems.isEmpty()) {
                            println("주의: 응답 데이터가 비어있습니다")
                            rankingError = "데이터가 없습니다."
                        } else {
                            // 상위 3개만 선택
                            stockRankings = rankingResponse.rankingItems.take(3)
                            rankingError = null
                        }
                    } ?: run {
                        println("응답 바디가 null입니다")
                        rankingError = "데이터가 없습니다."
                    }
                } else {
                    println("API 호출 실패")
                    println("응답 코드: ${response.code()}")
                    println("에러 메시지: ${response.errorBody()?.string()}")
                    rankingError = "데이터를 불러오는데 실패했습니다. (${response.code()})"
                }
            }
        } catch (e: Exception) {
            println("API 호출 중 예외 발생: ${e.message}")
            rankingError = e.message ?: "알 수 없는 오류가 발생했습니다."
        } finally {
            isLoadingRankings = false
        }
    }

    // 관심 종목 목록
    val watchlistItems = viewModel.watchlistItems.collectAsState().value
    var localWatchlist by remember { mutableStateOf(watchlistItems.take(3)) }

    // 서버 데이터가 바뀌면 로컬도 동기화
    LaunchedEffect(watchlistItems) {
        localWatchlist = watchlistItems.take(3)
    }

    SwipeRefresh(
        state = rememberSwipeRefreshState(isRefreshing),
        onRefresh = {
            isRefreshing = true
            // 1. 모든 상태 초기화 (기존 데이터 화면 즉시 제거)
            viewModel.clearAllStates()
            stockRankings = emptyList()
            // 2. 데이터 다시 로드
            scope.launch {
                try {
                    coroutineScope {
                        if (authViewModel.isLoggedIn.value) {
                            memberService?.let { service ->
                                viewModel.loadDashboard(service, context)
                                viewModel.loadUserProfile(service, context)
                            }
                            viewModel.loadWatchlist(context)
                        }
                        
                        // 캡처할 값들
                        val currentIndex = selectedRankingIndex
                        val currentSortTypeValue = currentSortType
                        
                        // 랭킹도 새로
                        val response = if (currentIndex in listOf(1, 2)) {
                            stockService.getFluctuationRateRanking(
                                marketType = "000",
                                sortType = currentSortTypeValue
                            )
                        } else if (currentIndex == 3) {
                            stockService.getVolumeRanking(
                                marketType = "000"
                            )
                        } else {
                            stockService.getTradeAmountRanking(
                                marketType = "000"
                            )
                        }
                        
                        if (response.isSuccessful) {
                            response.body()?.let { rankingResponse ->
                                stockRankings = rankingResponse.rankingItems.take(3)
                            }
                        }
                    }
                } finally {
                    isRefreshing = false
                }
            }
        }
    ) {
        Scaffold(
            containerColor = Color(0xFFF5F5F5), // 회색 배경으로 변경
            topBar = {
                // TopAppBar 제거
            }
        ) { innerPadding ->
            Column(
                modifier = Modifier
                    .fillMaxSize()
                    .padding(innerPadding)
            ) {
                LazyColumn(
                    modifier = Modifier.fillMaxWidth(),
                    contentPadding = PaddingValues(horizontal = 16.dp, vertical = 16.dp),
                    verticalArrangement = Arrangement.spacedBy(16.dp)
                ) {
                    // 로그인 상태에 따른 배너/자산 정보 영역
                    items(1) {
                        if (authViewModel.isLoggedIn.value) {
                            // 상단 여백 추가
                            Spacer(modifier = Modifier.height(12.dp))
                            // 프로필 정보 카드
                            Card(
                                modifier = Modifier.fillMaxWidth(),
                                colors = CardDefaults.cardColors(containerColor = Color.White),
                                shape = RoundedCornerShape(16.dp),
                                elevation = CardDefaults.cardElevation(defaultElevation = 0.dp)
                            ) {
                                Column(
                                    modifier = Modifier
                                        .fillMaxWidth()
                                        .padding(horizontal = 24.dp, vertical = 20.dp)
                                ) {
                                    // 오른쪽 상단 닉네임
                                    Row(
                                        modifier = Modifier.fillMaxWidth(),
                                        horizontalArrangement = Arrangement.End
                                    ) {
                                        Text(
                                            text = "${userProfile?.nickname ?: "-"}님의 지갑",
                                            fontSize = 14.sp,
                                            fontWeight = FontWeight.W500,
                                            color = Color(0xFF888888)
                                        )
                                    }
                                    Spacer(modifier = Modifier.height(4.dp))
                                    // 총 자산 + 수익률
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
                                                    text = dashboardData?.totalAsset?.toFormattedString() ?: "-",
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
                                        // 수익률
                                        val returnRate = dashboardData?.totalReturnRate ?: 0.0
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
                                    // 보유 현금
                                    Text(
                                        text = "보유 현금: ${dashboardData?.memberMoney?.toFormattedString() ?: "-"}원",
                                        fontSize = 15.sp,
                                        color = Color.Black,
                                        fontWeight = FontWeight.W600
                                    )
                                    Spacer(modifier = Modifier.height(2.dp))
                                    // 평가 금액
                                    Text(
                                        text = "평가 금액: ${dashboardData?.stockValuation?.toFormattedString() ?: "-"}원",
                                        fontSize = 15.sp,
                                        color = Color.Black,
                                        fontWeight = FontWeight.W600
                                    )
                                }
                            }
                            
                            Spacer(modifier = Modifier.height(16.dp))
                            
                            // 보유 주식 카드
                            Card(
                                modifier = Modifier.fillMaxWidth(),
                                colors = CardDefaults.cardColors(containerColor = Color.White),
                                shape = RoundedCornerShape(16.dp),
                                elevation = CardDefaults.cardElevation(defaultElevation = 0.dp)
                            ) {
                                Column(
                                    modifier = Modifier
                                        .fillMaxWidth()
                                        .padding(horizontal = 24.dp)
                                        .padding(top = 24.dp, bottom = 16.dp)
                                ) {
                                    Text(
                                        text = "보유 주식",
                                        fontSize = 18.sp,
                                        fontWeight = FontWeight.W700,
                                        fontFamily = SCDreamFontFamily
                                    )
                                    Spacer(modifier = Modifier.height(8.dp))
                                    if (dashboardData?.holdings.isNullOrEmpty()) {
                                        Text(
                                            text = "보유 주식이 없습니다.",
                                            fontSize = 14.sp,
                                            fontWeight = FontWeight.W500,
                                            fontFamily = SCDreamFontFamily,
                                            color = Color.Gray,
                                            modifier = Modifier.padding(vertical = 16.dp)
                                        )
                                    } else {
                                        dashboardData?.holdings?.take(3)?.forEach { holding ->
                                            Surface(
                                                modifier = Modifier
                                                    .fillMaxWidth()
                                                    .padding(vertical = 8.dp)
                                                    .clickable {
                                                        navController.navigate(
                                                            Screen.StockDetail.createRoute(
                                                                stockCode = holding.stockCode,
                                                                currentPrice = holding.currentPrice?.toString() ?: holding.valuationAmount.toString(),
                                                                fluctuationRate = holding.returnRate.toString()
                                                            )
                                                        ) {
                                                            launchSingleTop = true
                                                            restoreState = true
                                                        }
                                                    },
                                                color = Color.White,
                                                shadowElevation = 0.dp
                                            ) {
                                                Row(
                                                    modifier = Modifier
                                                        .fillMaxWidth()
                                                        .padding(4.dp),
                                                    horizontalArrangement = Arrangement.SpaceBetween,
                                                    verticalAlignment = Alignment.CenterVertically
                                                ) {
                                                    // 왼쪽: 로고와 종목명, 수량
                                                    Row(
                                                        verticalAlignment = Alignment.CenterVertically,
                                                        modifier = Modifier.weight(1f)
                                                    ) {
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
                                                        Column {
                                                            Text(
                                                                text = holding.stockName,
                                                                fontSize = 14.sp,
                                                                fontWeight = FontWeight.W700,
                                                                fontFamily = SCDreamFontFamily
                                                            )
                                                            Text(
                                                                text = "${holding.quantity}주",
                                                                fontSize = 12.sp,
                                                                fontWeight = FontWeight.W500,
                                                                color = Color.Gray,
                                                                fontFamily = SCDreamFontFamily
                                                            )
                                                        }
                                                    }
                                                    // 오른쪽: 평가금액과 수익률
                                                    Column(
                                                        horizontalAlignment = Alignment.End
                                                    ) {
                                                        Text(
                                                            text = "${holding.valuationAmount.toFormattedString()} 원",
                                                            fontSize = 14.sp,
                                                            fontWeight = FontWeight.W700,
                                                            fontFamily = SCDreamFontFamily
                                                        )
                                                        val profitLossAbs = kotlin.math.abs(holding.profitLoss)
                                                        val profitLossSign = if (holding.profitLoss >= 0) "+" else "-"
                                                        Text(
                                                            text = buildString {
                                                                append(profitLossSign)
                                                                append("${profitLossAbs.toFormattedString()}원")
                                                                append("(")
                                                                if (holding.returnRate >= 0) append("+")
                                                                append(String.format("%.2f", holding.returnRate))
                                                                append("%")
                                                                append(")")
                                                            },
                                                            color = when {
                                                                holding.returnRate > 0 -> Color.Red
                                                                holding.returnRate < 0 -> Color.Blue
                                                                else -> Color.Gray
                                                            },
                                                            fontSize = 12.sp,
                                                            fontWeight = FontWeight.W500,
                                                            fontFamily = SCDreamFontFamily
                                                        )
                                                    }
                                                }
                                            }
                                        }
                                        if (dashboardData?.holdings?.size ?: 0 >= 4) {
                                            MoreButton(
                                                onClick = {
                                                    navController.navigate("${Screen.MyPage.route}?tab=portfolio") {
                                                        launchSingleTop = true
                                                        popUpTo(Screen.Home.route)
                                                    }
                                                },
                                                text = "보유 주식 더보기",
                                                modifier = Modifier.padding(horizontal = 16.dp)
                                            )
                                        }
                                    }
                                }
                            }
                            
                            Spacer(modifier = Modifier.height(16.dp))
                            
                            // 미체결 주문 카드 (pendingOrders + dashboardData.pendingOrderAmount)
                            if ((pendingOrders.isNotEmpty()) || ((dashboardData?.pendingOrderAmount ?: 0) > 0)) {
                                Card(
                                    modifier = Modifier.fillMaxWidth(),
                                    colors = CardDefaults.cardColors(containerColor = Color.White),
                                    shape = RoundedCornerShape(16.dp),
                                    elevation = CardDefaults.cardElevation(defaultElevation = 0.dp)
                                ) {
                                    Column(
                                        modifier = Modifier
                                            .fillMaxWidth()
                                            .padding(horizontal = 24.dp)
                                            .padding(top = 24.dp, bottom = 16.dp)
                                    ) {
                                        Text("미체결 주문", fontSize = 18.sp, fontWeight = FontWeight.W700, fontFamily = SCDreamFontFamily)
                                        Spacer(modifier = Modifier.height(8.dp))
                                        if ((dashboardData?.pendingOrderAmount ?: 0) > 0) {
                                            Text(
                                                text = "매수 주문 대기 금액: ${dashboardData?.pendingOrderAmount?.toFormattedString() ?: "-"}원",
                                                fontSize = 14.sp,
                                                fontWeight = FontWeight.W500,
                                                fontFamily = SCDreamFontFamily,
                                                color = Color(0xFFFFA000),
                                                modifier = Modifier.padding(bottom = 8.dp)
                                            )
                                        }
                                        PendingOrdersSection(viewModel, authViewModel, context, navController)
                                    }
                                }
                                
                                Spacer(modifier = Modifier.height(16.dp))
                            }
                            
                            // 관심 종목 카드
                            Card(
                                modifier = Modifier.fillMaxWidth(),
                                colors = CardDefaults.cardColors(containerColor = Color.White),
                                shape = RoundedCornerShape(16.dp),
                                elevation = CardDefaults.cardElevation(defaultElevation = 0.dp)
                            ) {
                                Column(
                                    modifier = Modifier
                                        .fillMaxWidth()
                                        .padding(horizontal = 18.dp)
                                        .padding(top = 16.dp, bottom = 16.dp)
                                ) {
                                    // 관심 종목
                                    Text(
                                        text = "관심 종목",
                                        fontSize = 18.sp,
                                        fontWeight = FontWeight.W700,
                                        fontFamily = SCDreamFontFamily
                                    )
                                    Spacer(modifier = Modifier.height(12.dp))
                                    // 관심 종목 목록
                                    if (localWatchlist.isEmpty()) {
                                        Text(
                                            text = "관심 종목이 없습니다.",
                                            fontSize = 14.sp,
                                            fontWeight = FontWeight.W500,
                                            fontFamily = SCDreamFontFamily,
                                            color = Color.Gray,
                                            modifier = Modifier.padding(vertical = 16.dp)
                                        )
                                    } else {
                                        localWatchlist.forEach { item ->
                                            FavoriteStockItem(
                                                stock = FavoriteStock(
                                                    stockCode = getPureStockCode(item.stockCode).trim().uppercase(),
                                                    stockName = item.stockName,
                                                    currentPrice = item.currentPrice.toString(),
                                                    fluctuationRate = item.changeRate
                                                ),
                                                navController = navController,
                                                authViewModel = authViewModel,
                                                viewModel = viewModel,
                                                onRemove = {
                                                    // 1. 즉시 화면에서 제거
                                                    localWatchlist = localWatchlist.filter { it.stockCode != item.stockCode }
                                                    // 2. 서버 동기화
                                                    viewModel.removeFromWatchlist(
                                                        stockCode = item.stockCode,
                                                        context = context,
                                                        onSuccess = { viewModel.loadWatchlist(context) }
                                                    )
                                                }
                                            )
                                        }
                                        Spacer(modifier = Modifier.height(2.dp))
                                        if (watchlistItems.size >= 4) {
                                            MoreButton(
                                                onClick = {
                                                    navController.navigate("${Screen.MyPage.route}?tab=favorite") {
                                                        launchSingleTop = true
                                                        popUpTo(Screen.Home.route)
                                                    }
                                                },
                                                text = "관심 종목 더보기",
                                                modifier = Modifier.padding(horizontal = 16.dp)
                                            )
                                        }
                                    }
                                }
                            }
                        } else {
                            // 비로그인 상태: 기존 배너 표시
                            Card(
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .padding(top = 10.dp),
                                colors = CardDefaults.cardColors(containerColor = Color.White),
                                shape = RoundedCornerShape(16.dp),
                                elevation = CardDefaults.cardElevation(defaultElevation = 0.dp)
                            ) {
                                Column(
                                    modifier = Modifier
                                        .fillMaxWidth()
                                        .background(Color(0xFFFFFBEB))
                                        .padding(top = 24.dp, bottom = 24.dp, start = 24.dp, end = 24.dp)
                                ) {
                                Row(
                                    modifier = Modifier.fillMaxWidth(),
                                    verticalAlignment = Alignment.CenterVertically
                                ) {
                                    Column(modifier = Modifier.weight(1f)) {
                                        Text("투자의 시작", fontSize = 28.sp, fontWeight = FontWeight.W700, fontFamily = SCDreamFontFamily)
                                        Spacer(modifier = Modifier.height(12.dp))
                                        Text("치킨스톡과", fontSize = 28.sp, fontWeight = FontWeight.W700, fontFamily = SCDreamFontFamily, lineHeight = 38.sp)
                                        Text("함께", fontSize = 28.sp, fontWeight = FontWeight.W700, fontFamily = SCDreamFontFamily, lineHeight = 38.sp)
                                    }
                                    Icon(
                                        painter = painterResource(id = R.drawable.logo),
                                        contentDescription = "Logo",
                                        tint = Color.Unspecified,
                                        modifier = Modifier.size(120.dp)
                                    )
                                }
                                Row(
                                    modifier = Modifier
                                        .fillMaxWidth()
                                        .padding(top = 24.dp),
                                    horizontalArrangement = Arrangement.Center
                                ) {
                                    Button(
                                        onClick = { 
                                            navController.navigate(Screen.Login.route)
                                        },
                                        colors = ButtonDefaults.buttonColors(containerColor = Color(0xFFFDD141)),
                                        modifier = Modifier
                                            .fillMaxWidth()
                                            .height(50.dp),
                                        shape = RoundedCornerShape(12.dp),
                                        contentPadding = PaddingValues(vertical = 12.dp)
                                    ) {
                                        Text(
                                            "지금 시작하기", 
                                            color = Color.Black, 
                                            fontWeight = FontWeight.Bold, 
                                            fontFamily = SCDreamFontFamily, 
                                            fontSize = 18.sp
                                        )
                                    }
                                }
                            }
                            }
                        }
                    }

                    // 실시간 종목 랭킹 영역 (로그인 상태와 무관하게 표시)
                    items(1) {
                        Card(
                            modifier = Modifier.fillMaxWidth(),
                            colors = CardDefaults.cardColors(containerColor = Color.White),
                            shape = RoundedCornerShape(16.dp),
                            elevation = CardDefaults.cardElevation(defaultElevation = 0.dp)
                        ) {
                            Column(
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .padding(vertical = 24.dp, horizontal = 12.dp)
                            ) {
                            Text("실시간 종목 랭킹", fontSize = 18.sp, fontWeight = FontWeight.W700, fontFamily = SCDreamFontFamily)
                            Spacer(modifier = Modifier.height(16.dp))

                            // 정렬 기준 선택
                            SegmentedControl(
                                items = rankingOptions,
                                selectedIndex = selectedRankingIndex,
                                onSelectedIndexChange = { selectedRankingIndex = it },
                                modifier = Modifier.padding(bottom = 16.dp)
                            )

                            if (isLoadingRankings) {
                                Box(
                                    modifier = Modifier
                                        .fillMaxWidth()
                                        .height(200.dp),
                                    contentAlignment = Alignment.Center
                                ) {
                                    CircularProgressIndicator(color = Color(0xFFFFEB3B))
                                }
                            } else if (rankingError != null) {
                                Text(
                                    text = rankingError!!,
                                    color = Color.Red,
                                    modifier = Modifier.padding(vertical = 16.dp)
                                )
                            } else {
                                stockRankings.forEach { rankingItem ->
                                    StockListItem(
                                        stock = StockItem(
                                            stockCode = rankingItem.stockCode.split("_")[0],
                                            stockName = rankingItem.stockName,
                                            market = "",
                                            currentPrice = rankingItem.currentPrice.replace("""[+\-]""".toRegex(), ""),
                                            fluctuationRate = rankingItem.fluctuationRate,
                                            tradeAmount = when (selectedRankingIndex) {
                                                3 -> rankingItem.tradeVolume?.toString() ?: "0" // 거래량 탭일 때는 tradeVolume 사용
                                                1, 2 -> rankingItem.contractStrength ?: "0" // 급상승/급하락 탭
                                                else -> rankingItem.tradeAmount ?: "0" // 거래대금 탭
                                            }
                                        ),
                                        navController = navController,
                                        authViewModel = authViewModel,
                                        viewModel = viewModel,
                                        showContractStrength = selectedRankingIndex in listOf(1, 2), // 급상승/급하락 탭일 때 체결강도 표시
                                        showTradeVolume = selectedRankingIndex == 3 // 거래량 탭일 때 거래량 표시
                                    )
                                }
                            }

                            Spacer(modifier = Modifier.height(16.dp))

                            MoreButton(
                                onClick = {
                                    navController.navigate(Screen.Stock.route) {
                                        launchSingleTop = true
                                        popUpTo(Screen.Home.route)
                                    }
                                },
                                text = "더 많은 종목 보기",
                                modifier = Modifier.padding(horizontal = 16.dp)
                            )
                        }
                        }
                    }
                }
            }
        }
    }
}

// 종목 코드에서 _AL 등 접미사를 제거하는 함수
fun getPureStockCode(stockCode: String): String {
    return stockCode.substringBefore("_")
}

@Composable
fun PendingOrdersSection(viewModel: MainViewModel, authViewModel: AuthViewModel, context: Context, navController: NavHostController) {
    val pendingOrders by viewModel.pendingOrders.collectAsState()
    LaunchedEffect(authViewModel.isLoggedIn.value) {
        if (authViewModel.isLoggedIn.value) {
            val token = TokenManager.getInstance(context).getAccessToken()
            if (!token.isNullOrBlank()) {
                viewModel.loadPendingOrders("Bearer "+token)
            }
        }
    }
    if (pendingOrders.isNotEmpty()) {
        val maxShow = 3
        val showList = pendingOrders.take(maxShow)
        val extraCount = pendingOrders.size - maxShow
        Column(modifier = Modifier.fillMaxWidth()) {
            Spacer(modifier = Modifier.height(16.dp))
            showList.forEach { order ->
                Surface(
                    modifier = Modifier
                        .fillMaxWidth()
                        .padding(vertical = 4.dp),
                    color = Color.White,
                    shadowElevation = 0.dp
                ) {
                    Row(
                        modifier = Modifier
                            .fillMaxWidth()
                            .padding(8.dp),
                        horizontalArrangement = Arrangement.SpaceBetween,
                        verticalAlignment = Alignment.CenterVertically
                    ) {
                        // 왼쪽: 로고와 종목명, 수량
                        Row(
                            verticalAlignment = Alignment.CenterVertically,
                            modifier = Modifier.weight(1f)
                        ) {
                            AsyncImage(
                                model = "https://thumb.tossinvest.com/image/resized/96x0/https%3A%2F%2Fstatic.toss.im%2Fpng-icons%2Fsecurities%2Ficn-sec-fill-${getPureStockCode(order.stockCode)}.png",
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
                                    fontSize = 14.sp,
                                    fontWeight = FontWeight.W700,
                                    fontFamily = SCDreamFontFamily
                                )
                                Text(
                                    text = "${order.quantity}주",
                                    fontSize = 12.sp,
                                    fontWeight = FontWeight.W500,
                                    color = Color.Gray,
                                    fontFamily = SCDreamFontFamily
                                )
                            }
                        }
                        // 오른쪽: 목표가격
                        Column(
                            horizontalAlignment = Alignment.End
                        ) {
                            Text(
                                text = "${order.targetPrice.toFormattedString()} 원",
                                fontSize = 14.sp,
                                fontWeight = FontWeight.W700,
                                fontFamily = SCDreamFontFamily
                            )
                            Text(
                                text = if (order.orderType == "BUY") "매수 주문" else "매도 주문",
                                color = if (order.orderType == "BUY") Color.Red else Color.Blue,
                                fontSize = 12.sp,
                                fontWeight = FontWeight.W500,
                                fontFamily = SCDreamFontFamily
                            )
                        }
                    }
                }
            }
            if (extraCount > 0) {
                Spacer(modifier = Modifier.height(8.dp))
                Row(
                    modifier = Modifier.fillMaxWidth(),
                    horizontalArrangement = Arrangement.Center
                ) {
                    Text(
                        text = "외 ${extraCount}개",
                        fontSize = 13.sp,
                        color = Color.Gray
                    )
                }
            }
            Spacer(modifier = Modifier.height(8.dp))
            MoreButton(
                onClick = {
                    navController.navigate("${Screen.MyPage.route}?tab=trade&pending=true") {
                        launchSingleTop = true
                        popUpTo(Screen.Home.route)
                    }
                },
                text = "더 자세히 보기",
                modifier = Modifier
                    .fillMaxWidth()
                    .height(38.dp)
            )
        }
    }
}
