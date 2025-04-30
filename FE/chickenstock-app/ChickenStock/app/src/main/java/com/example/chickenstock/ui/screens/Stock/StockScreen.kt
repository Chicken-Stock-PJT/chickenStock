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
import com.example.chickenstock.viewmodel.AuthViewModel
import com.example.chickenstock.viewmodel.MainViewModel
import com.example.chickenstock.api.RetrofitClient
import com.example.chickenstock.api.StockService
import com.example.chickenstock.model.RankingItem
import androidx.compose.ui.platform.LocalContext
import androidx.compose.foundation.lazy.rememberLazyListState
import androidx.compose.runtime.derivedStateOf
import androidx.compose.runtime.remember
import kotlinx.coroutines.launch

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
    navController: NavHostController,
    authViewModel: AuthViewModel,
    viewModel: MainViewModel
) {
    var selectedMarketIndex by remember { mutableStateOf(0) }
    var selectedSortIndex by remember { mutableStateOf(0) }
    val sortOptions = listOf("거래대금", "급상승", "급하락", "거래량")
    val marketTypes = listOf("000", "001", "101")  // 전체, 코스피, 코스닥
    
    // API 엔드포인트 결정
    val currentEndpoint = remember(selectedSortIndex) {
        when (selectedSortIndex) {
            0 -> "stock/ranking/tradeAmount" // 거래대금
            1 -> "stock/ranking/fluctuationRate" // 급상승
            2 -> "stock/ranking/fluctuationRate" // 급하락
            3 -> "stock/ranking/volume" // 거래량
            else -> "stock/ranking/tradeAmount"
        }
    }

    // sortType 결정
    val currentSortType = remember(selectedSortIndex) {
        when (selectedSortIndex) {
            0 -> "1" // 거래대금
            1 -> "1" // 급상승 (상승률)
            2 -> "3" // 급하락 (하락률)
            3 -> "1" // 거래량 (기본값 1 사용)
            else -> "1"
        }
    }
    
    // API 상태 관리
    var stockRankings by remember { mutableStateOf<List<RankingItem>>(emptyList()) }
    var isLoading by remember { mutableStateOf(false) }
    var error by remember { mutableStateOf<String?>(null) }
    
    // API 서비스 생성
    val context = LocalContext.current
    val stockService = remember { RetrofitClient.getInstance(context).create(StockService::class.java) }
    
    // 스크롤 상태
    val listState = rememberLazyListState()
    val coroutineScope = rememberCoroutineScope()

    // 초기 데이터 로드
    LaunchedEffect(selectedMarketIndex, selectedSortIndex) {
        isLoading = true
        stockRankings = emptyList() // 초기화
        try {
            println("API 호출 시작")
            println("선택된 탭: ${sortOptions[selectedSortIndex]}")
            println("마켓 타입: ${marketTypes[selectedMarketIndex]}")
            println("정렬 타입: $currentSortType")
            
            val response = if (selectedSortIndex in listOf(1, 2)) {
                // 급상승/급하락의 경우
                println("급상승/급하락 API 호출")
                stockService.getFluctuationRateRanking(
                    marketType = marketTypes[selectedMarketIndex],
                    sortType = currentSortType
                )
            } else if (selectedSortIndex == 3) {
                // 거래량의 경우
                println("거래량 API 호출")
                stockService.getVolumeRanking(
                    marketType = marketTypes[selectedMarketIndex]
                )
            } else {
                // 거래대금의 경우
                println("거래대금 API 호출")
                stockService.getTradeAmountRanking(
                    marketType = marketTypes[selectedMarketIndex]
                )
            }

            if (response.isSuccessful) {
                println("API 응답 성공")
                response.body()?.let { rankingResponse ->
                    println("응답 데이터:")
                    println("- 아이템 개수: ${rankingResponse.rankingItems.size}")
                    println("- code: ${rankingResponse.code}")
                    println("- message: ${rankingResponse.message}")
                    
                    if (rankingResponse.rankingItems.isEmpty()) {
                        println("주의: 응답 데이터가 비어있습니다")
                    } else {
                        println("첫 번째 아이템 샘플:")
                        val firstItem = rankingResponse.rankingItems.first()
                        println("- stockCode: ${firstItem.stockCode}")
                        println("- stockName: ${firstItem.stockName}")
                        println("- currentPrice: ${firstItem.currentPrice}")
                        println("- fluctuationRate: ${firstItem.fluctuationRate}")
                        println("- contractStrength: ${firstItem.contractStrength}")
                        println("- tradeVolume: ${firstItem.tradeVolume}")
                        println("- tradeAmount: ${firstItem.tradeAmount}")
                    }
                    
                    stockRankings = rankingResponse.rankingItems
                } ?: run {
                    println("응답 바디가 null입니다")
                    error = "데이터가 없습니다."
                }
            } else {
                println("API 호출 실패")
                println("응답 코드: ${response.code()}")
                println("에러 메시지: ${response.errorBody()?.string()}")
                error = "데이터를 불러오는데 실패했습니다. (${response.code()})"
            }
        } catch (e: Exception) {
            println("예외 발생: ${e.javaClass.simpleName}")
            println("에러 메시지: ${e.message}")
            e.printStackTrace()
            error = e.message ?: "알 수 없는 오류가 발생했습니다."
        } finally {
            isLoading = false
        }
    }

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
            
            if (isLoading) {
                Box(
                    modifier = Modifier.fillMaxSize(),
                    contentAlignment = androidx.compose.ui.Alignment.Center
                ) {
                    CircularProgressIndicator(color = Color(0xFFFFEB3B))
                }
            } else if (error != null) {
                Text(
                    text = error!!,
                    color = Color.Red,
                    modifier = Modifier.padding(32.dp)
                )
            } else {
                LazyColumn(
                    state = listState,
                    modifier = Modifier
                        .fillMaxSize()
                        .padding(horizontal = 32.dp)
                ) {
                    items(stockRankings) { rankingItem ->
                        StockListItem(
                            stock = StockItem(
                                stockCode = rankingItem.stockCode.split("_")[0],
                                stockName = rankingItem.stockName,
                                market = "",
                                currentPrice = rankingItem.currentPrice.replace("""[+\-]""".toRegex(), ""),
                                fluctuationRate = rankingItem.fluctuationRate,
                                tradeAmount = when (selectedSortIndex) {
                                    3 -> rankingItem.tradeVolume?.toString() ?: "0" // 거래량 탭일 때는 tradeVolume 사용
                                    1, 2 -> rankingItem.contractStrength ?: "0" // 급상승/급하락 탭
                                    else -> rankingItem.tradeAmount ?: "0" // 거래대금 탭
                                }
                            ),
                            navController = navController,
                            authViewModel = authViewModel,
                            viewModel = viewModel
                        )
                    }

                    // 로딩 인디케이터
                    item {
                        if (isLoading) {
                            Box(
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .padding(16.dp),
                                contentAlignment = Alignment.Center
                            ) {
                                CircularProgressIndicator(color = Color(0xFFFFEB3B))
                            }
                        }
                    }
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