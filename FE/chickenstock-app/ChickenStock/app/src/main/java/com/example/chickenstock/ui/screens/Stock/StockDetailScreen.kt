package com.example.chickenstock.ui.screens.stock

import android.graphics.Color as AndroidColor
import androidx.compose.foundation.layout.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.KeyboardArrowLeft
import androidx.compose.material.icons.filled.Favorite
import androidx.compose.material.icons.outlined.FavoriteBorder
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.toArgb
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.ui.viewinterop.AndroidView
import androidx.navigation.NavController
import com.example.chickenstock.ui.components.StockItem
import com.example.chickenstock.ui.components.StockBidAskView
import com.example.chickenstock.ui.theme.SCDreamFontFamily
import com.example.chickenstock.ui.theme.ChartRed
import com.example.chickenstock.ui.theme.Gray500
import com.example.chickenstock.ui.theme.Gray700
import com.example.chickenstock.ui.theme.Gray0
import com.github.mikephil.charting.charts.LineChart
import com.github.mikephil.charting.data.*
import com.github.mikephil.charting.formatter.ValueFormatter
import com.github.mikephil.charting.highlight.Highlight
import com.github.mikephil.charting.listener.OnChartValueSelectedListener
import com.github.mikephil.charting.interfaces.datasets.ILineDataSet
import com.example.chickenstock.ui.components.ChartMarkerView
import com.example.chickenstock.ui.theme.Gray300
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.shape.CircleShape
import coil.compose.AsyncImage
import androidx.compose.ui.draw.clip
import androidx.compose.foundation.background
import androidx.compose.material.icons.filled.KeyboardArrowDown
import androidx.compose.material3.DropdownMenu
import androidx.compose.material3.DropdownMenuItem
import androidx.compose.runtime.mutableStateOf
import androidx.compose.foundation.clickable
import androidx.compose.foundation.border
import com.example.chickenstock.ui.theme.*
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.platform.LocalConfiguration
import androidx.compose.animation.core.Spring
import androidx.compose.animation.core.spring
import androidx.compose.animation.core.animateFloatAsState
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.animation.core.animateDpAsState
import androidx.compose.animation.core.tween
import androidx.compose.foundation.layout.BoxWithConstraints
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.height
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.width
import androidx.compose.material3.ModalBottomSheet
import androidx.compose.material3.rememberModalBottomSheetState
import com.github.mikephil.charting.charts.CandleStickChart
import com.github.mikephil.charting.data.CandleData
import com.github.mikephil.charting.data.CandleDataSet
import com.github.mikephil.charting.data.CandleEntry
import com.github.mikephil.charting.components.XAxis
import com.github.mikephil.charting.components.YAxis
import android.graphics.Paint
import com.github.mikephil.charting.data.Entry
import com.github.mikephil.charting.utils.MPPointF
import com.example.chickenstock.R
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.ui.text.input.KeyboardType
import androidx.compose.material.icons.filled.CheckCircle
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.OutlinedTextFieldDefaults
import androidx.compose.material3.Divider
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.ui.platform.LocalFocusManager
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.focus.focusRequester
import androidx.compose.ui.text.input.ImeAction
import androidx.compose.foundation.layout.imePadding
import androidx.compose.foundation.text.KeyboardActions
import androidx.compose.ui.platform.LocalSoftwareKeyboardController
import android.widget.Toast
import androidx.compose.ui.platform.LocalContext
import kotlinx.coroutines.MainScope
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import com.example.chickenstock.api.RetrofitClient
import com.example.chickenstock.api.StockService
import com.example.chickenstock.api.StockDetailResponse
import android.util.Log
import okhttp3.*
import com.google.gson.Gson
import org.json.JSONObject
import com.example.chickenstock.api.WebSocketManager
import com.example.chickenstock.api.StockPrice
import com.example.chickenstock.api.StockBidAsk
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.DisposableEffect
import com.example.chickenstock.api.ChartResponse
import com.example.chickenstock.api.ChartData
import com.github.mikephil.charting.components.MarkerView
import android.widget.TextView
import android.view.LayoutInflater
import android.content.Context
import android.view.MotionEvent
import com.github.mikephil.charting.listener.OnChartGestureListener
import com.github.mikephil.charting.listener.ChartTouchListener
import java.util.ArrayList
import com.example.chickenstock.viewmodel.MainViewModel
import com.example.chickenstock.model.Position
import com.example.chickenstock.viewmodel.AuthViewModel
import com.example.chickenstock.navigation.Screen
import com.example.chickenstock.api.MemberService
import com.example.chickenstock.api.SimpleProfileResponse
import com.example.chickenstock.api.BuyOrderRequest
import com.example.chickenstock.api.SellOrderRequest
import com.example.chickenstock.data.TokenManager
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Locale
import java.util.Calendar
import com.example.chickenstock.api.TradeExecution
import androidx.compose.foundation.BorderStroke
import androidx.compose.ui.res.painterResource
import androidx.compose.material.icons.filled.Close
import androidx.compose.material.icons.filled.Forum
import androidx.compose.material.icons.outlined.Forum
import androidx.compose.material.icons.outlined.Chat
import androidx.compose.material.icons.filled.ArrowDropDown

class CustomMarkerView(
    context: Context,
    layoutResource: Int,
    private val dates: List<String>
) : MarkerView(context, layoutResource) {
    private val tvDate: TextView = findViewById(R.id.tvDate)
    private val tvValue: TextView = findViewById(R.id.tvValue)

    override fun refreshContent(e: Entry?, highlight: Highlight?) {
        e?.let {
            val index = it.x.toInt()
            val date = if (index >= 0 && index < dates.size) dates[index] else ""
            tvDate.text = date
            tvValue.text = String.format("%,.0f원", it.y)
        }
        super.refreshContent(e, highlight)
    }

    override fun getOffset(): MPPointF {
        return MPPointF((-(width / 2)).toFloat(), (-height).toFloat())
    }
}

@Composable
fun OrderInfoRow(
    label: String,
    value: String,
    modifier: Modifier = Modifier
) {
    Row(
        modifier = modifier
            .fillMaxWidth()
            .padding(vertical = 4.dp),
        horizontalArrangement = Arrangement.SpaceBetween
    ) {
        Text(
            text = label,
            fontSize = 14.sp,
            color = Gray500,
            fontFamily = SCDreamFontFamily
        )
        Text(
            text = value,
            fontSize = 14.sp,
            color = Gray700,
            fontFamily = SCDreamFontFamily
        )
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun StockDetailScreen(
    navController: NavController,
    stock: StockItem,
    viewModel: MainViewModel,
    authViewModel: AuthViewModel
) {
    val context = LocalContext.current
    val token = com.example.chickenstock.data.TokenManager.getInstance(context).getAccessToken()
    var selectedPeriod by remember { mutableStateOf("일") }
    var selectedMinuteInterval by remember { mutableStateOf("1분") }
    var showMinuteDropdown by remember { mutableStateOf(false) }
    val minuteOptions = listOf("1분", "5분", "10분", "15분", "30분", "60분")
    val coroutineScope = rememberCoroutineScope()
    
    // 거래 가능 시간 체크
    val currentTime = remember { Calendar.getInstance() }
    val timeFormat = SimpleDateFormat("HH:mm", Locale.getDefault())
    val currentTimeStr = timeFormat.format(currentTime.time)
    val currentHour = currentTime.get(Calendar.HOUR_OF_DAY)
    val currentMinute = currentTime.get(Calendar.MINUTE)
    // 08:00~20:00 사이에만 거래 가능
    val isTradingHours = (currentHour > 8 || (currentHour == 8 && currentMinute >= 0)) && (currentHour < 20 || (currentHour == 20 && currentMinute == 0))
    
    // 거래 불가능 안내 다이얼로그 상태
    var showTradingHoursDialog by remember { mutableStateOf(false) }

    var showSellBottomSheet by remember { mutableStateOf(false) }
    var showBuyBottomSheet by remember { mutableStateOf(false) }
    var isShowingCandleChart by remember { mutableStateOf(false) }
    var isLoading by remember { mutableStateOf(false) }
    val bottomSheetState = rememberModalBottomSheetState(
        skipPartiallyExpanded = true,
        confirmValueChange = { true }
    )

    // 매수 관련 상태들
    var buyOrderType by remember { mutableStateOf("시장가") }
    var buyPrice by remember { mutableStateOf("") }
    var buyQuantity by remember { mutableStateOf("") }
    var buyStep by remember { mutableStateOf(1) }
    var showBuySuccessMessage by remember { mutableStateOf(false) }
    var buyFailedMessage by remember { mutableStateOf<String?>(null) }
    
    // 매도 관련 상태들
    var sellOrderType by remember { mutableStateOf("시장가") }
    var sellPrice by remember { mutableStateOf("") }
    var sellQuantity by remember { mutableStateOf("") }
    var sellStep by remember { mutableStateOf(1) }
    var showSellSuccessMessage by remember { mutableStateOf(false) }
    
    // API 상태 관리를 위한 변수 추가
    var userProfile by remember { mutableStateOf<SimpleProfileResponse?>(null) }
    var isProfileLoading by remember { mutableStateOf(false) }
    var profileError by remember { mutableStateOf<String?>(null) }
    var portfolioData by remember { mutableStateOf(viewModel.portfolioData.value) }
    var isPortfolioLoading by remember { mutableStateOf(false) }
    var portfolioError by remember { mutableStateOf<String?>(null) }

    // 현재 잔고 계산
    val currentBalance = userProfile?.memberMoney?.replace(",", "")?.toIntOrNull() ?: 0

    // 임시 보유 주식 데이터 (이름 변경)
    val holdingCurrentPrice = stock.currentPrice.toInt()
    val holdingQuantity = 100
    val averagePrice = 55000
    val profitRate = ((holdingCurrentPrice.toFloat() - averagePrice.toFloat()) / averagePrice.toFloat() * 100)
    val totalProfit = (holdingCurrentPrice.toLong() - averagePrice.toLong()) * holdingQuantity.toLong()

    // 키보드 컨트롤러를 최상단에서 선언
    val keyboardController = LocalSoftwareKeyboardController.current
    val focusManager = LocalFocusManager.current
    val priceFocusRequester = remember { FocusRequester() }
    val quantityFocusRequester = remember { FocusRequester() }

    // 키보드 숨기는 함수
    val hideKeyboard = {
        keyboardController?.hide()
        focusManager.clearFocus()
    }

    // API 상태 관리
    var stockDetail by remember { mutableStateOf<StockDetailResponse?>(null) }
    var chartData by remember { mutableStateOf<ChartResponse?>(null) }
    var isChartLoading by remember { mutableStateOf(false) }
    var nextKey by remember { mutableStateOf<String?>(null) }
    var hasNext by remember { mutableStateOf(true) }
    var isLoadingMoreData by remember { mutableStateOf(false) }

    // 웹소켓 데이터 상태
    val webSocketManager = remember { WebSocketManager.getInstance() }
    val stockPrice = webSocketManager.stockPrice.collectAsState().value
    val stockBidAsk = webSocketManager.stockBidAsk.collectAsState().value
    val tradeExecutions = webSocketManager.tradeExecutions.collectAsState().value

    // API 데이터 상태
    var apiStockPrice by remember { mutableStateOf<StockDetailResponse?>(null) }
    var apiStockBidAsk by remember { mutableStateOf<StockBidAsk?>(null) }
    var isLoadingPrice by remember { mutableStateOf(false) }
    var isLoadingBidAsk by remember { mutableStateOf(false) }
    
    // 에러 메시지 상태 변수 추가
    var errorMessage by remember { mutableStateOf<String?>(null) }

    // 현재 보유 주식 정보 찾기 (항상 최신 portfolioData 사용)
    val currentPosition = remember(stock.stockCode, portfolioData) {
        portfolioData?.positions?.find { position -> 
            position.stockCode == stock.stockCode 
        }
    }

    // 웹소켓 실시간 데이터 상태
    var currentPrice by remember { mutableStateOf(0) }
    var priceChange by remember { mutableStateOf(0) }
    var changeRate by remember { mutableStateOf(0.0) }
    var askPrices by remember { mutableStateOf<Map<String, String>>(emptyMap()) }
    var askVolumes by remember { mutableStateOf<Map<String, String>>(emptyMap()) }
    var bidPrices by remember { mutableStateOf<Map<String, String>>(emptyMap()) }
    var bidVolumes by remember { mutableStateOf<Map<String, String>>(emptyMap()) }

    // 웹소켓 연결 및 API 데이터 로드
    LaunchedEffect(stock.stockCode) {
        Log.d("StockDetailScreen", "초기 데이터 로드 시작: ${stock.stockCode}")
        
        // 1. 먼저 REST API로 데이터 로드
        isLoadingPrice = true
        isLoadingBidAsk = true
        try {
            val stockService = RetrofitClient.getInstance(context).create(StockService::class.java)
            
            // 현재가 조회
            val priceResponse = stockService.getStockInfo(stock.stockCode)
            if (priceResponse.isSuccessful) {
                apiStockPrice = priceResponse.body()
                Log.d("StockDetailScreen", "API 현재가 로드 성공: ${apiStockPrice?.currentPrice}")
                
                // 초기 현재가 설정
                currentPrice = apiStockPrice?.currentPrice?.replace("[+,-]".toRegex(), "")?.toIntOrNull() ?: 0
                priceChange = apiStockPrice?.priceChange?.replace(",", "")?.toIntOrNull() ?: 0 // 부호 살림
                changeRate = apiStockPrice?.changeRate?.replace("%", "")?.toDoubleOrNull() ?: 0.0 // 부호 살림
            } else {
                Log.e("StockDetailScreen", "API 현재가 로드 실패: ${priceResponse.code()}")
            }

            // 호가 조회
            val bidAskResponse = stockService.getStockBidAsk(stock.stockCode)
            if (bidAskResponse.isSuccessful && bidAskResponse.body() != null) {
                val response = bidAskResponse.body()!!
                
                // 매도 호가 및 수량 맵 생성
                val askPricesMap = mutableMapOf<String, String>()
                val askVolumesMap = mutableMapOf<String, String>()
                
                // 매도 10호가부터 1호가까지 (역순)
                listOf(
                    response.sel_10th_pre_bid to response.sel_10th_pre_req,
                    response.sel_9th_pre_bid to response.sel_9th_pre_req,
                    response.sel_8th_pre_bid to response.sel_8th_pre_req,
                    response.sel_7th_pre_bid to response.sel_7th_pre_req,
                    response.sel_6th_pre_bid to response.sel_6th_pre_req,
                    response.sel_5th_pre_bid to response.sel_5th_pre_req,
                    response.sel_4th_pre_bid to response.sel_4th_pre_req,
                    response.sel_3th_pre_bid to response.sel_3th_pre_req,
                    response.sel_2th_pre_bid to response.sel_2th_pre_req,
                    response.sel_fpr_bid to response.sel_fpr_req
                ).forEachIndexed { index, (price, volume) ->
                    val key = (10 - index).toString()
                    askPricesMap[key] = price.replace("""[+\-,\s]""".toRegex(), "")
                    askVolumesMap[key] = volume
                }

                // 매수 호가 및 수량 맵 생성
                val bidPricesMap = mutableMapOf<String, String>()
                val bidVolumesMap = mutableMapOf<String, String>()
                
                // 매수 1호가부터 10호가까지
                listOf(
                    response.buy_fpr_bid to response.buy_fpr_req,
                    response.buy_2th_pre_bid to response.buy_2th_pre_req,
                    response.buy_3th_pre_bid to response.buy_3th_pre_req,
                    response.buy_4th_pre_bid to response.buy_4th_pre_req,
                    response.buy_5th_pre_bid to response.buy_5th_pre_req,
                    response.buy_6th_pre_bid to response.buy_6th_pre_req,
                    response.buy_7th_pre_bid to response.buy_7th_pre_req,
                    response.buy_8th_pre_bid to response.buy_8th_pre_req,
                    response.buy_9th_pre_bid to response.buy_9th_pre_req,
                    response.buy_10th_pre_bid to response.buy_10th_pre_req
                ).forEachIndexed { index, (price, volume) ->
                    val key = (index + 1).toString()
                    bidPricesMap[key] = price.replace("""[+\-,\s]""".toRegex(), "")
                    bidVolumesMap[key] = volume
                }

                apiStockBidAsk = StockBidAsk(
                    type = "bidask",
                    stockCode = stock.stockCode,
                    timestamp = response.bid_req_base_tm,
                    askPrices = askPricesMap,
                    askVolumes = askVolumesMap,
                    bidPrices = bidPricesMap,
                    bidVolumes = bidVolumesMap
                )
                
                // API 데이터로 초기 호가 정보 설정
                askPrices = askPricesMap
                askVolumes = askVolumesMap
                bidPrices = bidPricesMap
                bidVolumes = bidVolumesMap
                
                Log.d("StockDetailScreen", "API 호가 로드 성공: ${askPricesMap.size} 매도, ${bidPricesMap.size} 매수")
            } else {
                Log.e("StockDetailScreen", "API 호가 로드 실패: ${bidAskResponse.code()}")
            }
            
            // 종목 상세 정보 로드
            try {
                val response = stockService.getStockDetail(stock.stockCode)
                if (response.isSuccessful) {
                    stockDetail = response.body()
                    Log.d("StockDetailScreen", "종목 상세 정보 로드 성공: ${stockDetail?.shortName}")
                }
            } catch (e: Exception) {
                Log.e("StockDetailScreen", "종목 상세 정보 로드 실패", e)
            }
        } catch (e: Exception) {
            Log.e("StockDetailScreen", "API 데이터 로드 실패", e)
        } finally {
            isLoadingPrice = false
            isLoadingBidAsk = false
        }
        
        // 2. 데이터 로드 후 웹소켓 연결
        Log.d("StockDetailScreen", "웹소켓 연결 시작: ${stock.stockCode}")
        try {
            webSocketManager.connect(stock.stockCode)
        } catch (e: Exception) {
            Log.e("StockDetailScreen", "웹소켓 연결 실패: ${e.message}", e)
        }
    }

    DisposableEffect(stock.stockCode) {
        onDispose { 
            Log.d("StockDetailScreen", "웹소켓 연결 해제")
            webSocketManager.disconnect() 
        }
    }

    // 웹소켓 데이터 수신 시 상태 업데이트
    LaunchedEffect(stockPrice) {
        stockPrice?.let { price ->
            Log.d("StockDetailScreen", "웹소켓 현재가 업데이트: ${price.currentPrice}")
            // 음수 값이 올 경우 절대값으로 변환하여 저장
            val parsedPrice = price.currentPrice.replace("""[+\-,]""".toRegex(), "").toIntOrNull() ?: currentPrice
            currentPrice = parsedPrice
            priceChange = price.priceChange.toIntOrNull() ?: priceChange
            changeRate = price.changeRate.toDoubleOrNull() ?: changeRate
            Log.d("StockDetailScreen", "변환된 현재가: $currentPrice")
        }
    }
    
    LaunchedEffect(stockBidAsk) {
        stockBidAsk?.let { bidAsk ->
            Log.d("StockDetailScreen", "웹소켓 호가 업데이트: ${bidAsk.timestamp}")
            askPrices = bidAsk.askPrices
            askVolumes = bidAsk.askVolumes
            bidPrices = bidAsk.bidPrices
            bidVolumes = bidAsk.bidVolumes
        }
    }

    // 차트 데이터 로드
    LaunchedEffect(selectedPeriod, selectedMinuteInterval) {
        isChartLoading = true
        try {
            val chartType = when (selectedPeriod) {
                "분" -> "MINUTE"
                "일" -> "DAILY"
                "주" -> "WEEKLY"
                "월" -> "MONTHLY"
                "년" -> "YEARLY"
                else -> "DAILY"
            }
            val timeInterval = if (selectedPeriod == "분") {
                when (selectedMinuteInterval) {
                    "1분" -> "1"
                    "5분" -> "5"
                    "10분" -> "10"
                    "15분" -> "15"
                    "30분" -> "30"
                    "60분" -> "60"
                    else -> "1"
                }
            } else null

            Log.d("StockDetailScreen", "차트 데이터 요청: chartType=$chartType, timeInterval=$timeInterval, stockCode=${stock.stockCode}")

            val stockService = RetrofitClient.getInstance(context).create(StockService::class.java)
            
            // 첫 번째 데이터 로드
            val initialResponse = stockService.getStockChart(
                chartType = chartType,
                stockCode = stock.stockCode,
                timeInterval = timeInterval,
                nextKey = null
            )
            
            val allChartData = initialResponse.chartData.toMutableList()
            var currentNextKey = initialResponse.nextKey
            var hasMoreData = initialResponse.hasNext
            
            // 년 차트는 데이터가 적어서 추가 로딩이 필요 없으나,
            // 다른 차트의 경우 모든 데이터를 가져오기 위해 nextKey가 있는 한 계속 로드
            if (selectedPeriod != "년" && hasMoreData && currentNextKey != null) {
                // 최대 10회까지 추가 데이터 로드 (성능 및 무한 루프 방지)
                for (i in 0 until 10) {
                    if (!hasMoreData || currentNextKey == null) break
                    
                    val additionalResponse = stockService.getStockChart(
                        chartType = chartType,
                        stockCode = stock.stockCode,
                        timeInterval = timeInterval,
                        nextKey = currentNextKey
                    )
                    
                    // 기존 데이터에 추가
                    allChartData.addAll(additionalResponse.chartData)
                    currentNextKey = additionalResponse.nextKey
                    hasMoreData = additionalResponse.hasNext
                }
            }
            
            // 통합된 데이터로 응답 생성
            chartData = ChartResponse(
                stockCode = stock.stockCode,
                chartType = chartType,
                chartData = allChartData,
                nextKey = currentNextKey,
                hasNext = hasMoreData,
                code = initialResponse.code,
                message = initialResponse.message
            )
            
            nextKey = currentNextKey
            hasNext = hasMoreData
            
            Log.d("StockDetailScreen", "차트 데이터 로드 성공: ${allChartData.size}개, nextKey=${currentNextKey}, hasNext=${hasMoreData}")
            
            // 데이터 상세 로그
            allChartData.take(3).forEachIndexed { index, data ->
                Log.d("StockDetailScreen", "첫 데이터 $index: date=${data.date}, currentPrice=${data.currentPrice}")
            }
            if (allChartData.size > 3) {
                val lastIndex = allChartData.size - 1
                Log.d("StockDetailScreen", "마지막 데이터: date=${allChartData[lastIndex].date}, currentPrice=${allChartData[lastIndex].currentPrice}")
            }
        } catch (e: Exception) {
            Log.e("StockDetailScreen", "차트 데이터 로드 실패", e)
            // Toast.makeText(context, "차트 데이터를 불러오는데 실패했습니다.", Toast.LENGTH_SHORT).show() // 제거
        } finally {
            isChartLoading = false
        }
    }

    // 차트 스크롤 처리
    var isScrolling by remember { mutableStateOf(false) }

    // 차트 스크롤 이벤트 처리
    val onChartScroll = { x: Float ->
        if (!isScrolling && !isLoadingMoreData && hasNext && x < 10) {
            isScrolling = true
            isLoadingMoreData = true
            
            coroutineScope.launch {
                try {
                    val chartType = when (selectedPeriod) {
                        "분" -> "MINUTE"
                        "일" -> "DAILY"
                        "주" -> "WEEKLY"
                        "월" -> "MONTHLY"
                        "년" -> "YEARLY"
                        else -> "DAILY"
                    }
                    
                    val timeInterval = if (selectedPeriod == "분") {
                        "1"
                    } else null

                    val stockService = RetrofitClient.getInstance(context).create(StockService::class.java)
                    val response = stockService.getStockChart(
                        chartType = chartType,
                        stockCode = stock.stockCode,
                        timeInterval = timeInterval,
                        nextKey = nextKey
                    )
                    
                    chartData = chartData?.copy(
                        chartData = chartData?.chartData.orEmpty() + response.chartData,
                        nextKey = response.nextKey,
                        hasNext = response.hasNext
                    )
                    nextKey = response.nextKey
                    hasNext = response.hasNext
                } catch (e: Exception) {
                    Log.e("StockDetailScreen", "추가 데이터 로드 실패", e)
                } finally {
                    isLoadingMoreData = false
                    isScrolling = false
                }
            }
        }
    }

    // 로그인 다이얼로그 상태
    var showLoginDialog by remember { mutableStateOf(false) }
    var loginDialogType by remember { mutableStateOf("") } // "buy", "sell", "favorite"

    // 관심종목 여부 확인
    val isInWatchlist = isInWatchlist(viewModel.watchlist.value, getPureStockCode(stock.stockCode))

    // 프로필 정보 로드
    LaunchedEffect(authViewModel.isLoggedIn.value) {
        if (authViewModel.isLoggedIn.value) {
            isProfileLoading = true
            isPortfolioLoading = true
            try {
                val memberService = RetrofitClient.getInstance(context).create(MemberService::class.java)
                val profileResponse = memberService.getSimpleProfile("Bearer $token")
                if (profileResponse.isSuccessful) {
                    userProfile = profileResponse.body()
                }
                val portfolioResponse = memberService.getPortfolio("Bearer $token")
                if (portfolioResponse.isSuccessful) {
                    portfolioData = portfolioResponse.body()
                }
            } catch (e: Exception) {
                profileError = e.message
                portfolioError = e.message
            } finally {
                isProfileLoading = false
                isPortfolioLoading = false
            }
        }
    }

    // 로그인 다이얼로그
    if (showLoginDialog) {
        AlertDialog(
            onDismissRequest = { showLoginDialog = false },
            title = {
                Text(
                    "로그인이 필요합니다",
                    fontFamily = SCDreamFontFamily,
                    color = Color.Black,
                    fontWeight = FontWeight.Bold
                )
            },
            text = {
                Text(
                    when (loginDialogType) {
                        "buy" -> "매수 주문을 위해서는 로그인이 필요합니다."
                        "sell" -> "매도 주문을 위해서는 로그인이 필요합니다."
                        "favorite" -> "관심 종목 등록을 위해서는 로그인이 필요합니다."
                        else -> "로그인이 필요합니다."
                    },
                    fontFamily = SCDreamFontFamily,
                    color = Color.Black
                )
            },
            confirmButton = {
                TextButton(
                    onClick = {
                        showLoginDialog = false
                        navController.navigate(Screen.Login.route)
                    }
                ) {
                    Text(
                        "로그인하기",
                        color = Color(0xFF0066CC),
                        fontFamily = SCDreamFontFamily,
                        fontWeight = FontWeight.Bold
                    )
                }
            },
            dismissButton = {
                TextButton(
                    onClick = { showLoginDialog = false }
                ) {
                    Text(
                        "취소",
                        color = Color(0xFF0066CC),
                        fontFamily = SCDreamFontFamily,
                        fontWeight = FontWeight.Bold
                    )
                }
            },
            containerColor = Color.White
        )
    }

    // 매도 바텀시트 열릴 때마다 최신 데이터 로드
    LaunchedEffect(showSellBottomSheet) {
        if (showSellBottomSheet && authViewModel.isLoggedIn.value) {
            isProfileLoading = true
            isPortfolioLoading = true
            try {
                val memberService = RetrofitClient.getInstance(context).create(MemberService::class.java)
                val profileResponse = memberService.getSimpleProfile("Bearer $token")
                if (profileResponse.isSuccessful) {
                    userProfile = profileResponse.body()
                }
                val portfolioResponse = memberService.getPortfolio("Bearer $token")
                if (portfolioResponse.isSuccessful) {
                    portfolioData = portfolioResponse.body()
                }
            } catch (e: Exception) {
                profileError = e.message
                portfolioError = e.message
            } finally {
                isProfileLoading = false
                isPortfolioLoading = false
            }
        }
    }
    // 매수 바텀시트 열릴 때마다 최신 데이터 로드
    LaunchedEffect(showBuyBottomSheet) {
        if (showBuyBottomSheet && authViewModel.isLoggedIn.value) {
            isProfileLoading = true
            isPortfolioLoading = true
            try {
                val memberService = RetrofitClient.getInstance(context).create(MemberService::class.java)
                val profileResponse = memberService.getSimpleProfile("Bearer $token")
                if (profileResponse.isSuccessful) {
                    userProfile = profileResponse.body()
                }
                val portfolioResponse = memberService.getPortfolio("Bearer $token")
                if (portfolioResponse.isSuccessful) {
                    portfolioData = portfolioResponse.body()
                }
            } catch (e: Exception) {
                profileError = e.message
                portfolioError = e.message
            } finally {
                isProfileLoading = false
                isPortfolioLoading = false
            }
        }
    }

    // 거래 시간 외 안내 다이얼로그
    if (showTradingHoursDialog) {
        AlertDialog(
            onDismissRequest = { showTradingHoursDialog = false },
            title = {
                Text(
                    "거래 시간 안내",
                    fontFamily = SCDreamFontFamily,
                    color = Color.Black,
                    fontWeight = FontWeight.Bold
                )
            },
            text = {
                Text(
                    "주식 거래는 매일 08:00~20:00에만 가능합니다. (현재 시간: $currentTimeStr)",
                    fontFamily = SCDreamFontFamily,
                    color = Color.Black,
                    textAlign = TextAlign.Center
                )
            },
            confirmButton = {
                TextButton(
                    onClick = { showTradingHoursDialog = false }
                ) {
                    Text(
                        "확인",
                        color = Color(0xFF0066CC),
                        fontFamily = SCDreamFontFamily,
                        fontWeight = FontWeight.Bold
                    )
                }
            },
            containerColor = Color.White
        )
    }

    if (showSellBottomSheet) {
        val scrollState = rememberScrollState()

        // 매도 바텀 시트 초기화
        LaunchedEffect(Unit) {
            sellPrice = (stockPrice?.currentPrice ?: stock.currentPrice).replace("""[+\-]""".toRegex(), "")
        }

        ModalBottomSheet(
            onDismissRequest = { 
                hideKeyboard()
                showSellBottomSheet = false
                sellStep = 1
                sellOrderType = "시장가"
                sellPrice = ""
                sellQuantity = ""
                showSellSuccessMessage = false
            },
            sheetState = bottomSheetState,
            containerColor = Gray0
        ) {
            Column(
                modifier = Modifier
                    .fillMaxWidth()
                    .imePadding()
                    .padding(16.dp)
            ) {
                when (sellStep) {
                    1 -> {
                        Column(
                            modifier = Modifier
                                .fillMaxWidth()
                                .verticalScroll(scrollState)
                        ) {
                            Row(
                                modifier = Modifier.fillMaxWidth(),
                                horizontalArrangement = Arrangement.SpaceBetween,
                                verticalAlignment = Alignment.CenterVertically
                            ) {
                                Text(
                                    text = "매도 주문",
                                    fontSize = 20.sp,
                                    fontWeight = FontWeight.Bold,
                                    fontFamily = SCDreamFontFamily,
                                    color = Gray700
                                )
                                Text(
                                    text = "수수료 0.015% + 제세금 0.18%",
                                    fontSize = 12.sp,
                                    color = Gray500,
                                    fontFamily = SCDreamFontFamily
                                )
                            }
                            Spacer(modifier = Modifier.height(16.dp))

                            // 시장가/지정가 선택 버튼
                            Row(
                                modifier = Modifier.fillMaxWidth(),
                                horizontalArrangement = Arrangement.Center
                            ) {
                                Button(
                                    onClick = { sellOrderType = "시장가" },
                                    colors = ButtonDefaults.buttonColors(
                                        containerColor = if (sellOrderType == "시장가") ChartBlue else Gray200 // 파란색
                                    ),
                                    modifier = Modifier.weight(1f)
                                ) {
                                    Text(
                                        text = "시장가",
                                        color = if (sellOrderType == "시장가") Color.White else Color.Black,
                                        fontWeight = FontWeight.Bold,
                                        fontFamily = SCDreamFontFamily
                                    )
                                }
                                Spacer(modifier = Modifier.width(8.dp))
                                Button(
                                    onClick = { sellOrderType = "지정가" },
                                    colors = ButtonDefaults.buttonColors(
                                        containerColor = if (sellOrderType == "지정가") ChartBlue else Gray200 // 파란색
                                    ),
                                    modifier = Modifier.weight(1f)
                                ) {
                                    Text(
                                        text = "지정가",
                                        color = if (sellOrderType == "지정가") Color.White else Color.Black,
                                        fontWeight = FontWeight.Bold,
                                        fontFamily = SCDreamFontFamily
                                    )
                                }
                            }
                            Spacer(modifier = Modifier.height(16.dp))

                            // 보유 주식 정보
                            Text(
                                text = "보유 수량",
                                fontSize = 14.sp,
                                color = Gray500,
                                fontFamily = SCDreamFontFamily
                            )
                            Text(
                                text = if (currentPosition != null) "${currentPosition.quantity}주" else "0주",
                                fontSize = 16.sp,
                                fontWeight = FontWeight.Bold,
                                color = Gray700,
                                fontFamily = SCDreamFontFamily
                            )

                            if (currentPosition != null) {
                                Spacer(modifier = Modifier.height(12.dp))
                                Text(
                                    text = "평균 매수가",
                                    fontSize = 14.sp,
                                    color = Gray500,
                                    fontFamily = SCDreamFontFamily
                                )
                                Text(
                                    text = "${String.format("%,d", currentPosition.averagePrice)}원",
                                    fontSize = 16.sp,
                                    fontWeight = FontWeight.Bold,
                                    color = Gray700,
                                    fontFamily = SCDreamFontFamily
                                )
                                Spacer(modifier = Modifier.height(12.dp))
                                Text(
                                    text = "수익률",
                                    fontSize = 14.sp,
                                    color = Gray500,
                                    fontFamily = SCDreamFontFamily
                                )
                                Text(
                                    text = String.format("%.2f%%", currentPosition.returnRate),
                                    fontSize = 16.sp,
                                    fontWeight = FontWeight.Bold,
                                    color = when {
                                        currentPosition.returnRate > 0f -> ChartRed
                                        currentPosition.returnRate < 0f -> ChartBlue
                                        else -> Gray700
                                    },
                                    fontFamily = SCDreamFontFamily
                                )
                                Spacer(modifier = Modifier.height(12.dp))
                                Text(
                                    text = "평가 손익",
                                    fontSize = 14.sp,
                                    color = Gray500,
                                    fontFamily = SCDreamFontFamily
                                )
                                Text(
                                    text = "${String.format("%,d", currentPosition.profitLoss)}원",
                                    fontSize = 16.sp,
                                    fontWeight = FontWeight.Bold,
                                    color = when {
                                        currentPosition.profitLoss > 0L -> ChartRed
                                        currentPosition.profitLoss < 0L -> ChartBlue
                                        else -> Gray700
                                    },
                                    fontFamily = SCDreamFontFamily
                                )
                            }
                            Spacer(modifier = Modifier.height(24.dp))

                            // 지정가일 때만 가격 입력, 최대 버튼, 총 매도 금액 표시
                            if (sellOrderType == "지정가") {
                                OutlinedTextField(
                                    value = sellPrice,
                                    onValueChange = { sellPrice = it },
                                    label = { Text("주문 가격", fontFamily = SCDreamFontFamily) },
                                    keyboardOptions = KeyboardOptions(
                                        keyboardType = KeyboardType.Number,
                                        imeAction = ImeAction.Next
                                    ),
                                    keyboardActions = KeyboardActions(
                                        onNext = {
                                            quantityFocusRequester.requestFocus()
                                        }
                                    ),
                                    modifier = Modifier
                                        .fillMaxWidth()
                                        .focusRequester(priceFocusRequester),
                                    colors = OutlinedTextFieldDefaults.colors(
                                        focusedBorderColor = ChartBlue,
                                        unfocusedBorderColor = Gray300,
                                        focusedTextColor = Color.Black,
                                        unfocusedTextColor = Color.Black,
                                        disabledTextColor = Color.Black,
                                        disabledBorderColor = Gray300,
                                        disabledLabelColor = Gray700
                                    )
                                )
                                Spacer(modifier = Modifier.height(8.dp))
                            }

                            // 수량 입력 (시장가/지정가 공통)
                            OutlinedTextField(
                                value = sellQuantity,
                                onValueChange = { sellQuantity = it },
                                label = { Text("주문 수량", fontFamily = SCDreamFontFamily) },
                                keyboardOptions = KeyboardOptions(
                                    keyboardType = KeyboardType.Number,
                                    imeAction = ImeAction.Done
                                ),
                                keyboardActions = KeyboardActions(
                                    onDone = {
                                        val quantity = sellQuantity.toIntOrNull() ?: 0
                                        val price = sellPrice.toLongOrNull()
                                        val isValid = if (sellOrderType == "시장가") {
                                            quantity > 0 && quantity <= (currentPosition?.quantity ?: 0)
                                        } else {
                                            quantity > 0 && quantity <= (currentPosition?.quantity ?: 0) && sellPrice.isNotEmpty() && price != null && price > 0
                                        }
                                        if (isValid) {
                                            hideKeyboard()
                                            sellStep = 2
                                            isLoading = true
                                            errorMessage = null
                                            showSellSuccessMessage = false
                                            coroutineScope.launch {
                                                try {
                                                    val stockService = RetrofitClient.getInstance(context).create(StockService::class.java)
                                                    val sellOrder = SellOrderRequest(
                                                        stockCode = stock.stockCode,
                                                        quantity = quantity,
                                                        price = if (sellOrderType == "지정가") price else null,
                                                        marketOrder = sellOrderType == "시장가"
                                                    )
                                                    Log.d("매도버튼", "sellOrder=$sellOrder, 주문유형=$sellOrderType")
                                                    val response = stockService.sellStock("Bearer $token", sellOrder)
                                                    Log.d("매도버튼", "API 응답: isSuccessful=${response.isSuccessful}, code=${response.code()}")
                                                    if (response.isSuccessful) {
                                                        delay(1000)
                                                        isLoading = false
                                                        showSellSuccessMessage = true
                                                        errorMessage = null
                                                        // 프로필 정보 새로고침 (잔고 업데이트를 위해)
                                                        val memberService = RetrofitClient.getInstance(context).create(MemberService::class.java)
                                                        val profileResponse = memberService.getSimpleProfile("Bearer $token")
                                                        if (profileResponse.isSuccessful) {
                                                            userProfile = profileResponse.body()
                                                        }
                                                        // 포트폴리오 정보 새로고침
                                                        val portfolioResponse = memberService.getPortfolio("Bearer $token")
                                                        if (portfolioResponse.isSuccessful) {
                                                            portfolioData = portfolioResponse.body()
                                                        }
                                                    } else {
                                                        val errorBody = response.errorBody()?.string()
                                                        Log.d("매도버튼", "API 실패 응답: $errorBody")
                                                        val errorMsg = try {
                                                            org.json.JSONObject(errorBody).optString("message")
                                                        } catch (e: Exception) { "매도에 실패했습니다." }
                                                        isLoading = false
                                                        showSellSuccessMessage = false
                                                        errorMessage = errorMsg.ifBlank { "매도에 실패했습니다." }
                                                    }
                                                } catch (e: Exception) {
                                                    Log.e("매도버튼", "코루틴 예외", e)
                                                    isLoading = false
                                                    showSellSuccessMessage = false
                                                    errorMessage = "매도에 실패했습니다."
                                                }
                                            }
                                        }
                                    }
                                ),
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .focusRequester(quantityFocusRequester),
                                colors = OutlinedTextFieldDefaults.colors(
                                    focusedBorderColor = ChartBlue,
                                    unfocusedBorderColor = Gray300,
                                    focusedTextColor = Color.Black,
                                    unfocusedTextColor = Color.Black
                                )
                            )
                            Spacer(modifier = Modifier.height(12.dp))
                            Row(
                                modifier = Modifier.fillMaxWidth(),
                                horizontalArrangement = Arrangement.spacedBy(8.dp)
                            ) {
                                if (sellOrderType == "시장가") {
                                    val increments = listOf(1, 5, 10, 100)
                                    increments.forEach { inc ->
                                        Button(
                                            onClick = {
                                                val current = sellQuantity.toIntOrNull() ?: 0
                                                val maxQty = currentPosition?.quantity ?: 0
                                                val newQty = (current + inc).coerceAtMost(maxQty)
                                                sellQuantity = newQty.toString()
                                            },
                                            colors = ButtonDefaults.buttonColors(containerColor = ChartBlue),
                                            shape = RoundedCornerShape(12.dp),
                                            modifier = Modifier.weight(1f)
                                        ) {
                                            Text(
                                                text = "+$inc",
                                                color = Color.White,
                                                fontWeight = FontWeight.Bold,
                                                fontFamily = SCDreamFontFamily,
                                                fontSize = 13.sp
                                            )
                                        }
                                    }
                                } else {
                                    val percentButtons = listOf(10, 25, 50)
                                    percentButtons.forEach { percent ->
                                        Button(
                                            onClick = {
                                                val holding = currentPosition?.quantity ?: 0
                                                if (holding > 0) {
                                                    val qty = (holding * percent / 100.0).toInt().coerceAtLeast(1)
                                                    sellQuantity = qty.toString()
                                                }
                                            },
                                            colors = ButtonDefaults.buttonColors(containerColor = ChartBlue),
                                            shape = RoundedCornerShape(12.dp),
                                            modifier = Modifier.weight(1f)
                                        ) {
                                            Text(
                                                text = "$percent%",
                                                color = Color.White,
                                                fontWeight = FontWeight.Bold,
                                                fontFamily = SCDreamFontFamily,
                                                fontSize = 13.sp
                                            )
                                        }
                                    }
                                    // 최대 버튼
                                    Button(
                                        onClick = {
                                            val holding = currentPosition?.quantity ?: 0
                                            if (holding > 0) {
                                                sellQuantity = holding.toString()
                                            }
                                        },
                                        colors = ButtonDefaults.buttonColors(containerColor = ChartBlue),
                                        shape = RoundedCornerShape(12.dp),
                                        modifier = Modifier.weight(1f)
                                    ) {
                                        Text(
                                            text = "최대",
                                            color = Color.White,
                                            fontWeight = FontWeight.Bold,
                                            fontFamily = SCDreamFontFamily,
                                            fontSize = 13.sp
                                        )
                                    }
                                }
                            }
                            Spacer(modifier = Modifier.height(16.dp))

                            // 수량 초과 시 경고 메시지
                            val quantity = sellQuantity.toIntOrNull() ?: 0
                            val currentHolding = currentPosition?.quantity ?: 0
                            if (quantity > currentHolding) {
                                Text(
                                    text = "보유 수량을 초과할 수 없습니다",
                                    color = ChartRed,
                                    fontSize = 12.sp,
                                    fontFamily = SCDreamFontFamily,
                                    modifier = Modifier.padding(top = 4.dp)
                                )
                            }

                            if (sellOrderType == "지정가") {
                                Spacer(modifier = Modifier.height(16.dp))
                                // 총 매도 금액
                                val totalAmount = try {
                                    val price = sellPrice.toLongOrNull() ?: 0L
                                    val sellQty = sellQuantity.toLongOrNull() ?: 0L
                                    price * sellQty
                                } catch (e: Exception) { 0L }
                                Text(
                                    text = "총 매도 금액",
                                    fontSize = 14.sp,
                                    color = Gray500,
                                    fontFamily = SCDreamFontFamily
                                )
                                Text(
                                    text = "${String.format("%,d", totalAmount)}원",
                                    fontSize = 16.sp,
                                    fontWeight = FontWeight.Bold,
                                    color = Gray700,
                                    fontFamily = SCDreamFontFamily
                                )
                                Spacer(modifier = Modifier.height(24.dp))
                            } else {
                                Spacer(modifier = Modifier.height(24.dp))
                            }

                            // 매도 버튼
                            Button(
                                onClick = {
                                    hideKeyboard()
                                    sellStep = 2
                                    isLoading = true
                                    errorMessage = null
                                    showSellSuccessMessage = false
                                    coroutineScope.launch {
                                        try {
                                            val stockService = RetrofitClient.getInstance(context).create(StockService::class.java)
                                            val sellOrder = SellOrderRequest(
                                                stockCode = stock.stockCode,
                                                quantity = sellQuantity.toInt(),
                                                price = if (sellOrderType == "지정가") sellPrice.toLongOrNull() else null,
                                                marketOrder = sellOrderType == "시장가"
                                            )
                                            Log.d("매도버튼", "sellOrder=$sellOrder, 주문유형=$sellOrderType")
                                            val response = stockService.sellStock("Bearer $token", sellOrder)
                                            Log.d("매도버튼", "API 응답: isSuccessful=${response.isSuccessful}, code=${response.code()}")
                                            if (response.isSuccessful) {
                                                delay(1000)
                                                isLoading = false
                                                showSellSuccessMessage = true
                                                errorMessage = null
                                                // 프로필 정보 새로고침 (잔고 업데이트를 위해)
                                                val memberService = RetrofitClient.getInstance(context).create(MemberService::class.java)
                                                val profileResponse = memberService.getSimpleProfile("Bearer $token")
                                                if (profileResponse.isSuccessful) {
                                                    userProfile = profileResponse.body()
                                                }
                                                // 포트폴리오 정보 새로고침
                                                val portfolioResponse = memberService.getPortfolio("Bearer $token")
                                                if (portfolioResponse.isSuccessful) {
                                                    portfolioData = portfolioResponse.body()
                                                }
                                            } else {
                                                val errorBody = response.errorBody()?.string()
                                                Log.d("매도버튼", "API 실패 응답: $errorBody")
                                                val errorMsg = try {
                                                    org.json.JSONObject(errorBody).optString("message")
                                                } catch (e: Exception) { "매도에 실패했습니다." }
                                                isLoading = false
                                                showSellSuccessMessage = false
                                                errorMessage = errorMsg.ifBlank { "매도에 실패했습니다." }
                                            }
                                        } catch (e: Exception) {
                                            Log.e("매도버튼", "코루틴 예외", e)
                                            isLoading = false
                                            showSellSuccessMessage = false
                                            errorMessage = "매도에 실패했습니다."
                                        }
                                    }
                                },
                                enabled = (sellOrderType == "시장가" && sellQuantity.isNotEmpty() && (sellQuantity.toIntOrNull() ?: 0) > 0 && (sellQuantity.toIntOrNull() ?: 0) <= (currentPosition?.quantity ?: 0)) ||
                                          (sellOrderType == "지정가" && sellPrice.isNotEmpty() && sellQuantity.isNotEmpty() && (sellQuantity.toIntOrNull() ?: 0) > 0 && (sellQuantity.toIntOrNull() ?: 0) <= (currentPosition?.quantity ?: 0)),
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .height(48.dp),
                                colors = ButtonDefaults.buttonColors(
                                    containerColor = ChartBlue, // 빨간색
                                    disabledContainerColor = Gray200
                                )
                            ) {
                                Text(
                                    text = "매도",
                                    fontSize = 16.sp,
                                    fontWeight = FontWeight.Bold,
                                    fontFamily = SCDreamFontFamily,
                                    color = Color.White
                                )
                            }
                        }
                    }
                    2 -> {
                        when {
                            isLoading -> {
                                Column(
                                    modifier = Modifier.fillMaxWidth(),
                                    horizontalAlignment = Alignment.CenterHorizontally
                                ) {
                                    CircularProgressIndicator(
                                        modifier = Modifier.size(48.dp),
                                        color = ChartBlue,
                                        strokeWidth = 3.dp
                                    )
                                    Spacer(modifier = Modifier.height(16.dp))
                                    Text(
                                        text = "주문 처리 중...",
                                        fontSize = 16.sp,
                                        fontFamily = SCDreamFontFamily,
                                        color = Gray700,
                                        textAlign = TextAlign.Center
                                    )
                                }
                            }
                            showSellSuccessMessage -> {
                                Column(
                                    modifier = Modifier.fillMaxWidth(),
                                    horizontalAlignment = Alignment.CenterHorizontally
                                ) {
                                    Icon(
                                        imageVector = Icons.Default.CheckCircle,
                                        contentDescription = "성공",
                                        tint = ChartBlue,
                                        modifier = Modifier.size(48.dp)
                                    )
                                    Spacer(modifier = Modifier.height(16.dp))
                                    Text(
                                        text = "주문이 완료되었습니다",
                                        fontSize = 20.sp,
                                        fontWeight = FontWeight.Bold,
                                        fontFamily = SCDreamFontFamily,
                                        color = Gray700,
                                        textAlign = TextAlign.Center
                                    )
                                    Spacer(modifier = Modifier.height(24.dp))
                                    Button(
                                        onClick = {
                                            hideKeyboard()
                                            showSellBottomSheet = false
                                            sellStep = 1
                                            sellOrderType = "시장가"
                                            sellPrice = ""
                                            sellQuantity = ""
                                            showSellSuccessMessage = false
                                            errorMessage = null
                                        },
                                        colors = ButtonDefaults.buttonColors(
                                            containerColor = ChartBlue
                                        ),
                                        modifier = Modifier.fillMaxWidth()
                                    ) {
                                        Text(
                                            text = "확인",
                                            fontSize = 16.sp,
                                            fontFamily = SCDreamFontFamily,
                                            color = Color.White
                                        )
                                    }
                                }
                            }
                            errorMessage != null -> {
                                Column(
                                    modifier = Modifier.fillMaxWidth(),
                                    horizontalAlignment = Alignment.CenterHorizontally
                                ) {
                                    Icon(
                                        imageVector = Icons.Default.Close, // X자 아이콘
                                        contentDescription = "실패",
                                        tint = ChartBlue,
                                        modifier = Modifier.size(48.dp)
                                    )
                                    Spacer(modifier = Modifier.height(16.dp))
                                    Text(
                                        text = errorMessage ?: "매도에 실패했습니다.",
                                        fontSize = 18.sp,
                                        fontWeight = FontWeight.Bold,
                                        fontFamily = SCDreamFontFamily,
                                        color = ChartBlue,
                                        textAlign = TextAlign.Center
                                    )
                                    Spacer(modifier = Modifier.height(24.dp))
                                    Button(
                                        onClick = {
                                            hideKeyboard()
                                            showSellBottomSheet = false
                                            sellStep = 1
                                            sellOrderType = "시장가"
                                            sellPrice = ""
                                            sellQuantity = ""
                                            showSellSuccessMessage = false
                                            errorMessage = null
                                        },
                                        colors = ButtonDefaults.buttonColors(
                                            containerColor = ChartBlue
                                        ),
                                        modifier = Modifier.fillMaxWidth()
                                    ) {
                                        Text(
                                            text = "확인",
                                            fontSize = 16.sp,
                                            fontFamily = SCDreamFontFamily,
                                            color = Color.White
                                        )
                                    }
                                }
                            }
                        }
                    }
                    3 -> {
                        // 주문 완료 화면
                        Column(
                            modifier = Modifier.fillMaxWidth(),
                            horizontalAlignment = Alignment.CenterHorizontally
                        ) {
                            Icon(
                                imageVector = Icons.Default.CheckCircle,
                                contentDescription = "성공",
                                tint = ChartBlue,
                                modifier = Modifier.size(48.dp)
                            )
                            
                            Spacer(modifier = Modifier.height(16.dp))
                            
                            Text(
                                text = "주문이 완료되었습니다",
                                fontSize = 20.sp,
                                fontWeight = FontWeight.Bold,
                                fontFamily = SCDreamFontFamily,
                                color = Gray700
                            )
                            
                            Spacer(modifier = Modifier.height(24.dp))
                            
                            Button(
                                onClick = { 
                                    hideKeyboard()
                                    showSellBottomSheet = false
                                    sellStep = 1
                                    sellOrderType = "시장가"
                                    sellPrice = ""
                                    sellQuantity = ""
                                    showSellSuccessMessage = false
                                },
                                colors = ButtonDefaults.buttonColors(
                                    containerColor = ChartBlue
                                ),
                                modifier = Modifier.fillMaxWidth()
                            ) {
                                Text(
                                    text = "확인",
                                    fontSize = 16.sp,
                                    fontFamily = SCDreamFontFamily,
                                    color = Color.White
                                )
                            }
                        }
                    }
                }
            }
        }
    }

    if (showBuyBottomSheet) {
        val scrollState = rememberScrollState()

        // 매수 바텀 시트 초기화
        LaunchedEffect(Unit) {
            buyPrice = (stockPrice?.currentPrice ?: stock.currentPrice).replace("""[+\-]""".toRegex(), "")
        }

        ModalBottomSheet(
            onDismissRequest = { 
                hideKeyboard()
                showBuyBottomSheet = false
                buyStep = 1
                buyOrderType = "시장가"
                buyPrice = ""
                buyQuantity = ""
                showBuySuccessMessage = false
            },
            sheetState = bottomSheetState,
            containerColor = Gray0
        ) {
            Column(
                modifier = Modifier
                    .fillMaxWidth()
                    .imePadding()
                    .padding(16.dp)
            ) {
                when (buyStep) {
                    1 -> {
                        Column(
                            modifier = Modifier
                                .fillMaxWidth()
                                .verticalScroll(scrollState)
                        ) {
                            Row(
                                modifier = Modifier.fillMaxWidth(),
                                horizontalArrangement = Arrangement.SpaceBetween,
                                verticalAlignment = Alignment.CenterVertically
                            ) {
                                Text(
                                    text = "매수 주문",
                                    fontSize = 20.sp,
                                    fontWeight = FontWeight.Bold,
                                    fontFamily = SCDreamFontFamily,
                                    color = Gray700
                                )
                                Text(
                                    text = "수수료 0.015%",
                                    fontSize = 12.sp,
                                    color = Gray500,
                                    fontFamily = SCDreamFontFamily
                                )
                            }
                            Spacer(modifier = Modifier.height(16.dp))

                            // 시장가/지정가 선택 버튼
                            Row(
                                modifier = Modifier.fillMaxWidth(),
                                horizontalArrangement = Arrangement.Center
                            ) {
                                Button(
                                    onClick = { buyOrderType = "시장가" },
                                    colors = ButtonDefaults.buttonColors(
                                        containerColor = if (buyOrderType == "시장가") ChartRed else Gray200
                                    ),
                                    modifier = Modifier.weight(1f)
                                ) {
                                    Text(
                                        text = "시장가",
                                        color = if (buyOrderType == "시장가") Color.White else Color.Black,
                                        fontWeight = FontWeight.Bold,
                                        fontFamily = SCDreamFontFamily
                                    )
                                }
                                Spacer(modifier = Modifier.width(8.dp))
                                Button(
                                    onClick = { buyOrderType = "지정가" },
                                    colors = ButtonDefaults.buttonColors(
                                        containerColor = if (buyOrderType == "지정가") ChartRed else Gray200
                                    ),
                                    modifier = Modifier.weight(1f)
                                ) {
                                    Text(
                                        text = "지정가",
                                        color = if (buyOrderType == "지정가") Color.White else Color.Black,
                                        fontWeight = FontWeight.Bold,
                                        fontFamily = SCDreamFontFamily
                                    )
                                }
                            }
                            Spacer(modifier = Modifier.height(16.dp))

                            // 현재 잔고 표시
                            Text(
                                text = "현재 잔고",
                                fontSize = 14.sp,
                                color = Gray500,
                                fontFamily = SCDreamFontFamily
                            )
                            if (isProfileLoading) {
                                CircularProgressIndicator(
                                    modifier = Modifier.size(16.dp),
                                    color = Secondary500
                                )
                            } else {
                                Text(
                                    text = if (userProfile?.memberMoney == null || userProfile?.memberMoney == "조회 중") "조회 중" else "${String.format("%,d", userProfile?.memberMoney?.replace(",", "")?.toIntOrNull() ?: 0)}원",
                                    fontSize = 16.sp,
                                    fontWeight = FontWeight.Bold,
                                    color = Gray700,
                                    fontFamily = SCDreamFontFamily
                                )
                            }
                            Spacer(modifier = Modifier.height(24.dp))
                            
                            // 시장가일 때 가격 입력, 총 주문 금액, 최대 버튼 숨김
                            if (buyOrderType == "지정가") {
                                // 가격 입력
                            OutlinedTextField(
                                value = buyPrice,
                                    onValueChange = { buyPrice = it },
                                label = { Text("주문 가격", fontFamily = SCDreamFontFamily) },
                                keyboardOptions = KeyboardOptions(
                                    keyboardType = KeyboardType.Number,
                                    imeAction = ImeAction.Next
                                ),
                                keyboardActions = KeyboardActions(
                                    onNext = {
                                        quantityFocusRequester.requestFocus()
                                    }
                                ),
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .focusRequester(priceFocusRequester),
                                colors = OutlinedTextFieldDefaults.colors(
                                    focusedBorderColor = ChartBlue,
                                    unfocusedBorderColor = Gray300,
                                    focusedTextColor = Color.Black,
                                    unfocusedTextColor = Color.Black,
                                    disabledTextColor = Color.Black,
                                    disabledBorderColor = Gray300,
                                    disabledLabelColor = Gray700
                                )
                            )
                            Spacer(modifier = Modifier.height(8.dp))
                            }
                            
                            // 수량 입력 (시장가/지정가 공통)
                            Row(
                                modifier = Modifier.fillMaxWidth(),
                                horizontalArrangement = Arrangement.spacedBy(8.dp),
                                verticalAlignment = Alignment.CenterVertically
                            ) {
                                OutlinedTextField(
                                    value = buyQuantity,
                                    onValueChange = { newValue ->
                                        // 숫자만 허용
                                        if (newValue.isEmpty() || newValue.all { it.isDigit() }) {
                                            buyQuantity = newValue
                                        }
                                    },
                                    label = { Text("주문 수량", fontFamily = SCDreamFontFamily) },
                                    keyboardOptions = KeyboardOptions(
                                        keyboardType = KeyboardType.Number,
                                        imeAction = ImeAction.Done
                                    ),
                                    keyboardActions = KeyboardActions(
                                        onDone = {
                                            val quantity = buyQuantity.toIntOrNull() ?: 0
                                            val price = buyPrice.toLongOrNull()
                                            val isValid = if (buyOrderType == "시장가") {
                                                quantity > 0
                                            } else {
                                                quantity > 0 && buyPrice.isNotEmpty() && price != null && price > 0 && (price * quantity) <= currentBalance
                                            }
                                            if (isValid) {
                                                hideKeyboard()
                                                buyStep = 3
                                                isLoading = true
                                                coroutineScope.launch {
                                                    try {
                                                        val stockService = RetrofitClient.getInstance(context).create(StockService::class.java)
                                                        val buyOrder = BuyOrderRequest(
                                                            stockCode = stock.stockCode,
                                                            quantity = quantity,
                                                            price = if (buyOrderType == "지정가") price else null,
                                                            marketOrder = buyOrderType == "시장가"
                                                        )
                                                        Log.d("매수버튼", "buyOrder=$buyOrder, 주문유형=$buyOrderType")
                                                        val response = stockService.buyStock("Bearer $token", buyOrder)
                                                        Log.d("매수버튼", "API 응답: isSuccessful=${response.isSuccessful}, code=${response.code()}")
                                                        if (response.isSuccessful) {
                                                            delay(1000)
                                                            isLoading = false
                                                            showBuySuccessMessage = true
                                                            buyFailedMessage = null
                                                            // 프로필 정보 새로고침 (잔고 업데이트를 위해)
                                                            val memberService = RetrofitClient.getInstance(context).create(MemberService::class.java)
                                                            val profileResponse = memberService.getSimpleProfile("Bearer $token")
                                                            if (profileResponse.isSuccessful) {
                                                                userProfile = profileResponse.body()
                                                            }
                                                        } else {
                                                            val errorBody = response.errorBody()?.string()
                                                            Log.d("매수버튼", "API 실패 응답: $errorBody")
                                                            val errorMsg = try {
                                                                org.json.JSONObject(errorBody).optString("message")
                                                            } catch (e: Exception) { "결제에 실패했습니다." }
                                                            isLoading = false
                                                            showBuySuccessMessage = false
                                                            buyFailedMessage = errorMsg.ifBlank { "결제에 실패했습니다." }
                                                        }
                                                    } catch (e: Exception) {
                                                        Log.e("매수버튼", "코루틴 예외", e)
                                                        isLoading = false
                                                        showBuySuccessMessage = false
                                                        buyFailedMessage = "결제에 실패했습니다."
                                                    }
                                                }
                                            }
                                        }
                                    ),
                                    modifier = Modifier
                                        .weight(1f)
                                        .focusRequester(quantityFocusRequester),
                                    colors = OutlinedTextFieldDefaults.colors(
                                        focusedBorderColor = ChartBlue,
                                        unfocusedBorderColor = Gray300,
                                        focusedTextColor = Color.Black,
                                        unfocusedTextColor = Color.Black
                                    )
                                )
                                
                            }
                            Spacer(modifier = Modifier.height(16.dp))
                            Row(
                                modifier = Modifier.fillMaxWidth(),
                                horizontalArrangement = Arrangement.spacedBy(8.dp)
                            ) {
                                if (buyOrderType == "시장가") {
                                    val increments = listOf(1, 5, 10, 100)
                                    increments.forEach { inc ->
                                        Button(
                                            onClick = {
                                                val current = buyQuantity.toIntOrNull() ?: 0
                                                buyQuantity = (current + inc).toString()
                                            },
                                            colors = ButtonDefaults.buttonColors(containerColor = ChartRed),
                                            shape = RoundedCornerShape(12.dp),
                                            modifier = Modifier.weight(1f)
                                        ) {
                                            Text(
                                                text = "+$inc",
                                                color = Color.White,
                                                fontWeight = FontWeight.Bold,
                                                fontFamily = SCDreamFontFamily,
                                                fontSize = 13.sp
                                            )
                                        }
                                    }
                                } else {
                                    val percentButtons = listOf(10, 25, 50)
                                    percentButtons.forEach { percent ->
                                        Button(
                                            onClick = {
                                                val price = buyPrice.toLongOrNull() ?: 0L
                                                val balance = userProfile?.memberMoney?.replace(",", "")?.toLongOrNull() ?: 0L
                                                if (price > 0L && balance > 0L) {
                                                    val amount = (balance * percent) / 100L
                                                    val qty = amount / price
                                                    buyQuantity = qty.toString()
                                                }
                                            },
                                            colors = ButtonDefaults.buttonColors(containerColor = ChartRed),
                                            shape = RoundedCornerShape(12.dp),
                                            modifier = Modifier.weight(1f)
                                        ) {
                                            Text(
                                                text = "${percent}%",
                                                color = Color.White,
                                                fontWeight = FontWeight.Bold,
                                                fontFamily = SCDreamFontFamily,
                                                fontSize = 13.sp
                                            )
                                        }
                                    }
                                    // 최대 버튼도 Long으로 변경
                                    Button(
                                        onClick = {
                                            val price = buyPrice.toLongOrNull() ?: 0L
                                            val balance = userProfile?.memberMoney?.replace(",", "")?.toLongOrNull() ?: 0L
                                            if (price > 0L && balance > 0L) {
                                                val qty = balance / price
                                                buyQuantity = qty.toString()
                                            }
                                        },
                                        colors = ButtonDefaults.buttonColors(containerColor = ChartRed),
                                        shape = RoundedCornerShape(12.dp),
                                        modifier = Modifier.weight(1f)
                                    ) {
                                        Text(
                                            text = "최대",
                                            color = Color.White,
                                            fontWeight = FontWeight.Bold,
                                            fontFamily = SCDreamFontFamily,
                                            fontSize = 13.sp
                                        )
                                    }
                                }
                            }
                            if (buyOrderType == "지정가") {
                            Spacer(modifier = Modifier.height(16.dp))
                            // 총 주문 금액
                            val totalAmount = try {
                                val price = buyPrice.toLongOrNull() ?: 0L
                                val quantity = buyQuantity.toLongOrNull() ?: 0L
                                price * quantity
                            } catch (e: Exception) { 0L }
                            Text(
                                text = "총 주문 금액",
                                fontSize = 14.sp,
                                color = Gray500,
                                fontFamily = SCDreamFontFamily
                            )
                            Text(
                                text = "${String.format("%,d", totalAmount)}원",
                                fontSize = 16.sp,
                                fontWeight = FontWeight.Bold,
                                color = if (totalAmount > currentBalance) ChartRed else Gray700,
                                fontFamily = SCDreamFontFamily
                            )
                            if (totalAmount > currentBalance) {
                                Text(
                                    text = "잔액이 부족합니다",
                                    fontSize = 12.sp,
                                    color = ChartRed,
                                    fontFamily = SCDreamFontFamily
                                )
                            }
                            Spacer(modifier = Modifier.height(24.dp))
                        } else {
                        Spacer(modifier = Modifier.height(24.dp))
                            }
                            
                            // 수량 입력 필드 아래 에러 메시지
                            if (buyQuantity.isNotEmpty() && (buyQuantity.toIntOrNull() ?: 0) <= 0) {
                                Text(
                                    text = "1주 이상의 수량을 입력해주세요",
                                    color = ChartRed,
                                    fontSize = 12.sp,
                                    fontFamily = SCDreamFontFamily,
                                    modifier = Modifier.padding(start = 4.dp, top = 4.dp)
                                )
                            }
                            // 버튼 Row 추가: 입력란 아래
                            Spacer(modifier = Modifier.height(8.dp))
                            // 매수 버튼
                            Button(
                                onClick = { 
                                    hideKeyboard()
                                    val quantity = buyQuantity.toIntOrNull() ?: 0
                                    if (quantity > 0) {
                                        buyStep = 3
                                        isLoading = true
                                        coroutineScope.launch {
                                            try {
                                                val stockService = RetrofitClient.getInstance(context).create(StockService::class.java)
                                                val buyOrder = BuyOrderRequest(
                                                    stockCode = stock.stockCode,
                                                    quantity = quantity,
                                                    price = if (buyOrderType == "지정가") buyPrice.toLongOrNull() else null,
                                                    marketOrder = buyOrderType == "시장가"
                                                )
                                                Log.d("매수버튼", "buyOrder=$buyOrder, 주문유형=$buyOrderType")
                                                val response = stockService.buyStock("Bearer $token", buyOrder)
                                                Log.d("매수버튼", "API 응답: isSuccessful=${response.isSuccessful}, code=${response.code()}")
                                                if (response.isSuccessful) {
                                                    delay(1000)
                                                    isLoading = false
                                                    showBuySuccessMessage = true
                                                    buyFailedMessage = null
                                                    
                                                    // 프로필 정보 새로고침 (잔고 업데이트를 위해)
                                                    val memberService = RetrofitClient.getInstance(context).create(MemberService::class.java)
                                                    val profileResponse = memberService.getSimpleProfile("Bearer $token")
                                                    if (profileResponse.isSuccessful) {
                                                        userProfile = profileResponse.body()
                                                    }
                                                } else {
                                                    val errorBody = response.errorBody()?.string()
                                                    Log.d("매수버튼", "API 실패 응답: $errorBody")
                                                    val errorMsg = try {
                                                        org.json.JSONObject(errorBody).optString("message")
                                                    } catch (e: Exception) { "결제에 실패했습니다." }
                                                    isLoading = false
                                                    showBuySuccessMessage = false
                                                    buyFailedMessage = errorMsg.ifBlank { "결제에 실패했습니다." }
                                                }
                                            } catch (e: Exception) {
                                                Log.e("매수버튼", "코루틴 예외", e)
                                                isLoading = false
                                                showBuySuccessMessage = false
                                                buyFailedMessage = "결제에 실패했습니다."
                                            }
                                        }
                                    }
                                },
                                enabled = buyQuantity.isNotEmpty() && 
                                          (buyQuantity.toIntOrNull() ?: 0) > 0 && 
                                          (buyOrderType != "지정가" || (
                                              buyPrice.isNotEmpty() &&
                                              try { buyPrice.toInt() > 0 } catch (_: Exception) { false } &&
                                              try {
                                                  val totalAmount = (buyPrice.toLongOrNull() ?: 0L) * (buyQuantity.toLongOrNull() ?: 0L)
                                                  totalAmount <= currentBalance
                                              } catch (_: Exception) { false }
                                          )),
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .height(48.dp),
                                colors = ButtonDefaults.buttonColors(
                                    containerColor = ChartRed,
                                    disabledContainerColor = Gray200
                                )
                            ) {
                                    Text(
                                    text = "매수",
                                        fontSize = 16.sp,
                                    fontWeight = FontWeight.Bold,
                                        fontFamily = SCDreamFontFamily,
                                        color = Color.White
                                    )
                                }
                            }
                            
                        }
                    2 -> {
                        // 지정가 주문 확인 화면
                        Column(
                            modifier = Modifier
                                .fillMaxWidth()
                                .verticalScroll(scrollState)
                        ) {
                            Text(
                                text = "매수 주문 확인",
                                fontSize = 20.sp,
                                fontWeight = FontWeight.Bold,
                                fontFamily = SCDreamFontFamily,
                                color = Gray700
                            )
                            
                            Spacer(modifier = Modifier.height(24.dp))
                            
                            // 주문 정보 표시
                            if (isLoading) {
                                Box(
                                    modifier = Modifier
                                        .fillMaxWidth()
                                        .height(200.dp),
                                    contentAlignment = Alignment.Center
                                ) {
                                    CircularProgressIndicator(
                                        modifier = Modifier.size(48.dp),
                                        color = ChartRed,
                                        strokeWidth = 3.dp
                                    )
                                }
                            } else {
                                Column(
                                    modifier = Modifier
                                        .fillMaxWidth()
                                        .background(Gray50, RoundedCornerShape(8.dp))
                                        .padding(16.dp)
                                ) {
                                    OrderInfoRow("종목명", stockDetail?.shortName ?: stock.stockName)
                                    OrderInfoRow("주문 유형", buyOrderType)
                                    OrderInfoRow("주문 가격", "${String.format("%,d", buyPrice.toIntOrNull() ?: 0)}원")
                                    OrderInfoRow("주문 수량", "${buyQuantity}주")
                                    OrderInfoRow("총 매수 금액", "${String.format("%,d", (buyPrice.toLongOrNull() ?: 0L) * (buyQuantity.toLongOrNull() ?: 0L))}원")
                                    Divider(modifier = Modifier.padding(vertical = 8.dp), color = Gray200)
                                    OrderInfoRow("현재 잔고", "${String.format("%,d", currentBalance)}원")
                                    OrderInfoRow(
                                        "매수 후 잔고", 
                                        "${String.format("%,d", currentBalance - ((buyPrice.toLongOrNull() ?: 0L) * (buyQuantity.toLongOrNull() ?: 0L)))}원"
                                    )
                                }
                            }
                            
                            Spacer(modifier = Modifier.height(24.dp))
                            
                            // 취소/확인 버튼
                            Row(
                                modifier = Modifier.fillMaxWidth(),
                                horizontalArrangement = Arrangement.spacedBy(8.dp)
                            ) {
                                // 취소 버튼 - 로딩 중일 때는 숨김
                                if (!isLoading) {
                                    Button(
                                        onClick = { 
                                            hideKeyboard()
                                            showBuyBottomSheet = false
                                            buyStep = 1
                                            buyOrderType = "시장가"
                                            buyPrice = ""
                                            buyQuantity = ""
                                        },
                                        colors = ButtonDefaults.buttonColors(
                                            containerColor = Gray200,
                                            contentColor = Gray700
                                        ),
                                        modifier = Modifier.weight(1f)
                                    ) {
                                        Text(
                                            text = "취소",
                                            fontSize = 16.sp,
                                            fontFamily = SCDreamFontFamily
                                        )
                                    }
                                }
                                
                                // 확인 버튼 - 항상 표시되지만 로딩 중일 때는 인디케이터로 변경
                                Button(
                                    onClick = { 
                                        hideKeyboard()
                                        isLoading = true
                                        // API 호출
                                        coroutineScope.launch {
                                            try {
                                                val stockService = RetrofitClient.getInstance(context).create(StockService::class.java)
                                                val buyOrder = BuyOrderRequest(
                                                    stockCode = stock.stockCode,
                                                    quantity = buyQuantity.toInt(),
                                                    price = buyPrice.toLongOrNull(),
                                                    marketOrder = false
                                                )
                                                Log.d("매수버튼", "지정가 buyOrder=$buyOrder")
                                                val response = stockService.buyStock("Bearer $token", buyOrder)
                                                
                                                if (response.isSuccessful) {
                                                    Log.d("매수버튼", "지정가 주문 성공")
                                                    showBuySuccessMessage = true
                                                    delay(500)
                                                    isLoading = false
                                                    buyStep = 3
                                                    
                                                    // 프로필 정보 새로고침 (잔고 업데이트를 위해)
                                                    val memberService = RetrofitClient.getInstance(context).create(MemberService::class.java)
                                                    val profileResponse = memberService.getSimpleProfile("Bearer $token")
                                                    if (profileResponse.isSuccessful) {
                                                        userProfile = profileResponse.body()
                                                    }
                                                } else {
                                                    val errorBody = response.errorBody()?.string()
                                                    Log.e("매수버튼", "지정가 주문 실패: $errorBody")
                                                    val errorMsg = try {
                                                        org.json.JSONObject(errorBody).optString("message")
                                                    } catch (e: Exception) { "주문에 실패했습니다." }
                                                    isLoading = false
                                                    showBuySuccessMessage = false
                                                    buyFailedMessage = errorMsg.ifBlank { "주문에 실패했습니다." }
                                                }
                                            } catch (e: Exception) {
                                                Log.e("매수버튼", "지정가 주문 중 오류 발생", e)
                                                isLoading = false
                                                showBuySuccessMessage = false
                                                buyFailedMessage = "주문 중 오류가 발생했습니다."
                                            }
                                        }
                                    },
                                    colors = ButtonDefaults.buttonColors(
                                        containerColor = ChartRed
                                    ),
                                    modifier = Modifier.weight(1f),
                                    enabled = !isLoading
                                ) {
                                    if (isLoading) {
                                        CircularProgressIndicator(
                                            modifier = Modifier.size(24.dp),
                                            color = Color.White,
                                            strokeWidth = 2.dp
                                        )
                                    } else {
                                        Text(
                                            text = "확인",
                                            fontSize = 16.sp,
                                            fontFamily = SCDreamFontFamily,
                                            color = Color.White
                                        )
                                    }
                                }
                            }
                        }
                    }
                    3 -> {
                        // 주문 완료/실패 화면
                        when {
                            isLoading -> {
                                Column(
                                    modifier = Modifier.fillMaxWidth(),
                                    horizontalAlignment = Alignment.CenterHorizontally
                                ) {
                                    CircularProgressIndicator(
                                        modifier = Modifier.size(48.dp),
                                        color = ChartRed,
                                        strokeWidth = 3.dp
                                    )
                                    Spacer(modifier = Modifier.height(16.dp))
                                    Text(
                                        text = "주문 처리 중...",
                                        fontSize = 16.sp,
                                        fontFamily = SCDreamFontFamily,
                                        color = Gray700,
                                        textAlign = TextAlign.Center
                                    )
                                }
                            }
                            showBuySuccessMessage == true -> {
                                Column(
                                    modifier = Modifier.fillMaxWidth(),
                                    horizontalAlignment = Alignment.CenterHorizontally
                                ) {
                                    Icon(
                                        imageVector = Icons.Default.CheckCircle,
                                        contentDescription = "성공",
                                        tint = ChartRed,
                                        modifier = Modifier.size(48.dp)
                                    )
                                    Spacer(modifier = Modifier.height(16.dp))
                                    Text(
                                        text = "주문이 완료되었습니다",
                                        fontSize = 20.sp,
                                        fontWeight = FontWeight.Bold,
                                        fontFamily = SCDreamFontFamily,
                                        color = Gray700,
                                        textAlign = TextAlign.Center
                                    )
                                    Spacer(modifier = Modifier.height(24.dp))
                                    Button(
                                        onClick = { 
                                            hideKeyboard()
                                            showBuyBottomSheet = false
                                            buyStep = 1
                                            buyOrderType = "시장가"
                                            buyPrice = ""
                                            buyQuantity = ""
                                            showBuySuccessMessage = false
                                            buyFailedMessage = null
                                        },
                                        colors = ButtonDefaults.buttonColors(
                                            containerColor = ChartRed
                                        ),
                                        modifier = Modifier.fillMaxWidth()
                                    ) {
                                        Text(
                                            text = "확인",
                                            fontSize = 16.sp,
                                            fontFamily = SCDreamFontFamily,
                                            color = Color.White
                                        )
                                    }
                                }
                            }
                            buyFailedMessage != null -> {
                                Column(
                                    modifier = Modifier.fillMaxWidth(),
                                    horizontalAlignment = Alignment.CenterHorizontally
                                ) {
                                    Icon(
                                        imageVector = Icons.Default.Close, // X자 아이콘
                                        contentDescription = "실패",
                                        tint = ChartRed,
                                        modifier = Modifier.size(48.dp)
                                    )
                                    Spacer(modifier = Modifier.height(16.dp))
                                    Text(
                                        text = buyFailedMessage ?: "결제에 실패했습니다.",
                                        fontSize = 18.sp,
                                        fontWeight = FontWeight.Bold,
                                        fontFamily = SCDreamFontFamily,
                                        color = ChartRed,
                                        textAlign = TextAlign.Center
                                    )
                                    Spacer(modifier = Modifier.height(24.dp))
                                    Button(
                                        onClick = { 
                                            hideKeyboard()
                                            showBuyBottomSheet = false
                                            buyStep = 1
                                            buyOrderType = "시장가"
                                            buyPrice = ""
                                            buyQuantity = ""
                                            showBuySuccessMessage = false
                                            buyFailedMessage = null
                                        },
                                        colors = ButtonDefaults.buttonColors(
                                            containerColor = ChartRed
                                        ),
                                        modifier = Modifier.fillMaxWidth()
                                    ) {
                                        Text(
                                            text = "확인",
                                            fontSize = 16.sp,
                                            fontFamily = SCDreamFontFamily,
                                            color = Color.White
                                        )
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    Scaffold(
        containerColor = Color(0xFFF5F5F5), // 홈과 동일한 연한 회색 배경 적용
        topBar = {
            TopAppBar(
                title = { },
                navigationIcon = {
                    IconButton(
                        onClick = { navController.navigateUp() },
                        modifier = Modifier
                            .size(72.dp)
                            .padding(start = 0.dp)
                    ) {
                        Icon(
                            imageVector = Icons.Filled.KeyboardArrowLeft,
                            contentDescription = "뒤로가기",
                            modifier = Modifier.size(52.dp),
                            tint = Gray700
                        )
                    }
                },
                actions = {
                    // 커뮤니티 버튼 (Outlined Chat 아이콘)
                    IconButton(
                        onClick = {
                            navController.navigate("chat/${stock.stockCode}")
                        },
                        modifier = Modifier
                            .size(48.dp)
                            .padding(end = 0.dp)
                    ) {
                        Icon(
                            imageVector = Icons.Outlined.Chat,
                            contentDescription = "커뮤니티",
                            tint = Gray700,
                            modifier = Modifier.size(28.dp)
                        )
                    }
                    // 하트 아이콘
                    IconButton(
                        onClick = {
                            if (authViewModel.isLoggedIn.value) {
                                // 현재 관심 종목 여부에 따라 API 호출
                                if (isInWatchlist) {
                                    // 관심 종목에서 제거
                                    viewModel.removeFromWatchlist(
                                        stockCode = getPureStockCode(stock.stockCode),
                                        context = context,
                                        onSuccess = { 
                                            // 관심 종목 목록 새로고침하여 즉시 UI에 반영
                                            viewModel.loadWatchlist(context)
                                        },
                                        onError = { errorMsg ->
                                            // 오류 발생 시 메시지 표시
                                            Toast.makeText(context, errorMsg, Toast.LENGTH_SHORT).show()
                                        }
                                    )
                                } else {
                                    // 관심 종목에 추가
                                    viewModel.addToWatchlist(
                                        stockCode = getPureStockCode(stock.stockCode),
                                        context = context,
                                        onSuccess = {
                                            // 관심 종목 목록 새로고침하여 즉시 UI에 반영
                                            viewModel.loadWatchlist(context)
                                        },
                                        onError = { errorMsg ->
                                            // 오류 발생 시 메시지 표시
                                            Toast.makeText(context, errorMsg, Toast.LENGTH_SHORT).show()
                                        }
                                    )
                                }
                            } else {
                                // 로그인이 필요한 경우 안내
                                showLoginDialog = true
                            }
                        },
                        modifier = Modifier
                            .size(48.dp)
                            .padding(end = 16.dp)
                    ) {
                        Icon(
                            imageVector = if (isInWatchlist) Icons.Filled.Favorite else Icons.Outlined.FavoriteBorder,
                            contentDescription = if (isInWatchlist) "관심 종목에서 제거" else "관심 종목에 추가",
                            tint = if (isInWatchlist) Color(0xFFFF4081) else Gray300,
                            modifier = Modifier.size(28.dp)
                        )
                    }
                },
                colors = TopAppBarDefaults.topAppBarColors(
                    containerColor = Color(0xFFF5F5F5) // 홈과 동일한 연한 회색 배경 적용
                ),
                modifier = Modifier.height(64.dp)
            )
        },
        bottomBar = {
            Row(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(16.dp)
                    .height(40.dp),
                horizontalArrangement = Arrangement.SpaceBetween
            ) {
                // 매수 버튼 (왼쪽)
                Button(
                    onClick = {
                        if (!isTradingHours) {
                            showTradingHoursDialog = true
                        } else if (authViewModel.isLoggedIn.value) {
                            showBuyBottomSheet = true
                        } else {
                            loginDialogType = "buy"
                            showLoginDialog = true
                        }
                    },
                    modifier = Modifier
                        .weight(1f)
                        .fillMaxHeight(),
                    colors = ButtonDefaults.buttonColors(
                        containerColor = if (isTradingHours) ChartRed else Gray300,
                        disabledContainerColor = Gray300
                    ),
                    shape = RoundedCornerShape(8.dp),
                    enabled = true
                ) {
                    Text(
                        text = "매수",
                        fontSize = 14.sp,
                        fontWeight = FontWeight.Bold,
                        fontFamily = SCDreamFontFamily,
                        color = Color.White
                    )
                }

                Spacer(modifier = Modifier.width(12.dp))

                // 매도 버튼 (오른쪽)
                Button(
                    onClick = {
                        if (!isTradingHours) {
                            showTradingHoursDialog = true
                        } else if (authViewModel.isLoggedIn.value) {
                            showSellBottomSheet = true
                        } else {
                            loginDialogType = "sell"
                            showLoginDialog = true
                        }
                    },
                    modifier = Modifier
                        .weight(1f)
                        .fillMaxHeight(),
                    colors = ButtonDefaults.buttonColors(
                        containerColor = if (isTradingHours) ChartBlue else Gray300,
                        disabledContainerColor = Gray300
                    ),
                    shape = RoundedCornerShape(8.dp),
                    enabled = true
                ) {
                    Text(
                        text = "매도",
                        fontSize = 14.sp,
                        fontWeight = FontWeight.W700,
                        fontFamily = SCDreamFontFamily,
                        color = Color.White
                    )
                }
            }
        }
    ) { paddingValues ->
        Column(
            modifier = Modifier
                .fillMaxSize()
                .background(Color(0xFFF5F5F5)) // 컨텐츠 전체에 배경색 확실히 적용
                .padding(paddingValues)
        ) {
            // 상단 고정 부분 (종목 이름, 가격 등)
            Row(
                verticalAlignment = Alignment.Top,
                modifier = Modifier
                    .background(Color(0xFFF5F5F5)) // 상단 Row에도 배경색 적용
                    .padding(start = 32.dp, top = 20.dp, end = 32.dp, bottom = 10.dp)
            ) {
                AsyncImage(
                    model = "https://thumb.tossinvest.com/image/resized/96x0/https%3A%2F%2Fstatic.toss.im%2Fpng-icons%2Fsecurities%2Ficn-sec-fill-${getPureStockCode(stock.stockCode).trim().uppercase()}.png",
                    contentDescription = "주식 로고",
                    modifier = Modifier
                        .size(46.dp)
                        .clip(RoundedCornerShape(12.dp))
                        .background(Color.White),
                    error = painterResource(id = com.example.chickenstock.R.drawable.logo)
                )
                Spacer(modifier = Modifier.width(12.dp))
                Column {
                    Text(
                        text = stockDetail?.shortName ?: stock.stockName,
                        fontSize = 16.sp,
                        fontWeight = FontWeight.W700,
                        fontFamily = SCDreamFontFamily
                    )
                    Row(
                        verticalAlignment = Alignment.CenterVertically,
                    ) {
                        // 현재가 표시 - 상태 변수 사용
                        Text(
                            text = "${String.format("%,d", currentPrice)}원",
                            fontSize = 12.sp,
                            fontWeight = FontWeight.W500,
                            fontFamily = SCDreamFontFamily
                        )
                        
                        Text(
                            text = " 어제보다 ",
                            fontSize = 12.sp,
                            fontWeight = FontWeight.W500,
                            color = Color.Gray,
                            fontFamily = SCDreamFontFamily
                        )

                        // 전일대비와 등락률 표시 - 상태 변수 사용
                        val isPositive = priceChange > 0 || (priceChange == 0 && changeRate > 0)
                        
                        Text(
                            text = "${if (isPositive) "+" else "-"}${String.format("%,d", Math.abs(priceChange))}원",
                            fontSize = 12.sp,
                            fontWeight = FontWeight.W500,
                            color = if (isPositive) Color.Red else Color.Blue,
                            fontFamily = SCDreamFontFamily
                        )
                        
                        Text(
                            text = " (${if (isPositive) "+" else "-"}${String.format("%.2f", Math.abs(changeRate))}%)",
                            fontSize = 12.sp,
                            fontWeight = FontWeight.W500,
                            color = if (isPositive) Color.Red else Color.Blue,
                            fontFamily = SCDreamFontFamily
                        )
                    }
                }
            }

            // 스크롤 가능한 부분
            LazyColumn(
                modifier = Modifier
                    .fillMaxSize()
                    .padding(horizontal = 32.dp)
            ) {
                // 차트 아이템
                item {
                    Card(
                        modifier = Modifier
                            .fillMaxWidth()
                            .height(400.dp)
                            .padding(vertical = 16.dp),
                        colors = CardDefaults.cardColors(containerColor = Color.White),
                        border = BorderStroke(1.dp, Gray300)
                    ) {
                        Column(
                            modifier = Modifier
                                .fillMaxSize()
                                .padding(top = 16.dp, bottom = 24.dp) // 하단 패딩 추가
                        ) {
                            // 기간 선택 버튼들
                            Row(
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .padding(bottom = 8.dp),
                                horizontalArrangement = Arrangement.End
                            ) {
                                // 기간 버튼들: 분/일/주/월/년
                                val periods = listOf("분", "일", "주", "월", "년")
                                periods.forEach { period ->
                                    if (period == "분") {
                                        Box {
                                            Row(
                                                modifier = Modifier
                                                    .clip(RoundedCornerShape(8.dp))
                                                    .background(if (selectedPeriod == period) Gray200 else Color.Transparent)
                                                    .clickable { showMinuteDropdown = true },
                                                verticalAlignment = Alignment.CenterVertically
                                            ) {
                                                Text(
                                                    text = if (selectedPeriod == "분") selectedMinuteInterval else period,
                                                    fontSize = 12.sp,
                                                    color = Color.Black,
                                                    fontFamily = SCDreamFontFamily,
                                                    modifier = Modifier.padding(horizontal = 8.dp, vertical = 6.dp)
                                                )
                                                Icon(
                                                    imageVector = Icons.Filled.ArrowDropDown,
                                                    contentDescription = "분 단위 선택",
                                                    tint = Color.Black,
                                                    modifier = Modifier.size(18.dp)
                                                )
                                            }
                                            DropdownMenu(
                                                expanded = showMinuteDropdown,
                                                onDismissRequest = { showMinuteDropdown = false },
                                                containerColor = Color.White,
                                            ) {
                                                minuteOptions.forEach { option ->
                                                    DropdownMenuItem(
                                                        text = { Text(option, fontFamily = SCDreamFontFamily, color = Color.Black) },
                                                        onClick = {
                                                            selectedMinuteInterval = option
                                                            showMinuteDropdown = false
                                                            selectedPeriod = "분"
                                                        }
                                                    )
                                                }
                                            }
                                        }
                                        Spacer(modifier = Modifier.width(8.dp))
                                    } else {
                                        Text(
                                            text = period,
                                            fontSize = 12.sp,
                                            color = Color.Black,
                                            fontFamily = SCDreamFontFamily,
                                            modifier = Modifier
                                                .clip(RoundedCornerShape(8.dp))
                                                .background(if (selectedPeriod == period) Gray200 else Color.Transparent)
                                                .clickable {
                                                    selectedPeriod = period
                                                    selectedMinuteInterval = "1분"
                                                }
                                                .padding(horizontal = 12.dp, vertical = 6.dp)
                                        )
                                        Spacer(modifier = Modifier.width(8.dp))
                                    }
                                }
                            }

                            Box(
                                modifier = Modifier
                                    .fillMaxSize()
                                    .weight(1f)
                                    .padding(horizontal = 8.dp, vertical = 8.dp),
                                contentAlignment = Alignment.Center
                            ) {
                                if (isChartLoading) {
                                    CircularProgressIndicator(
                                        modifier = Modifier.size(48.dp),
                                        color = Secondary500
                                    )
                                } else if (chartData?.chartData.isNullOrEmpty()) {
                                    Text(
                                        text = "차트 데이터가 없습니다",
                                        fontSize = 16.sp,
                                        color = Gray500,
                                        fontFamily = SCDreamFontFamily
                                    )
                                } else {
                                    // 캔들 차트
                                    AndroidView(
                                        modifier = Modifier.fillMaxSize(),
                                        factory = { ctx ->
                                            CandleStickChart(ctx).apply {
                                                description.isEnabled = false
                                                legend.isEnabled = false
                                                
                                                // 터치 관련 설정
                                                setTouchEnabled(true)
                                                isHighlightPerDragEnabled = true
                                                isHighlightPerTapEnabled = true
                                                marker = null
                                                setDrawMarkers(false)
                                                
                                                // 스크롤/줌 설정
                                                setScaleEnabled(true)
                                                setDrawGridBackground(false)
                                                isDragEnabled = true
                                                isScaleXEnabled = true
                                                isScaleYEnabled = true
                                                
                                                // 스크롤 동작 개선
                                                setDragDecelerationEnabled(true)
                                                setDragDecelerationFrictionCoef(0.9f)
                                                
                                                // 차트 표시 설정 - 최대 표시 캔들 개수를 증가
                                                setVisibleXRangeMaximum(100f)  // 한 번에 보이는 캔들 개수 증가
                                                setVisibleXRangeMinimum(5f)    // 최소 보이는 캔들 개수 감소
                                                
                                                // 차트 여백 설정
                                                setViewPortOffsets(120f, 20f, 40f, 20f)
                                                
                                                // 스크롤 이벤트 리스너 추가
                                                setOnChartGestureListener(object : OnChartGestureListener {
                                                    override fun onChartScale(me: MotionEvent?, scaleX: Float, scaleY: Float) {}
                                                    override fun onChartFling(me1: MotionEvent?, me2: MotionEvent?, velocityX: Float, velocityY: Float) {}
                                                    override fun onChartSingleTapped(me: MotionEvent?) {}
                                                    override fun onChartDoubleTapped(me: MotionEvent?) {}
                                                    override fun onChartLongPressed(me: MotionEvent?) {}
                                                    override fun onChartTranslate(me: MotionEvent?, dX: Float, dY: Float) {
                                                        onChartScroll(dX)
                                                    }
                                                    override fun onChartGestureStart(me: MotionEvent?, lastPerformedGesture: ChartTouchListener.ChartGesture?) {}
                                                    override fun onChartGestureEnd(me: MotionEvent?, lastPerformedGesture: ChartTouchListener.ChartGesture?) {}
                                                })

                                                // 데이터를 오래된 순으로 정렬하여 차트 오른쪽에 최신 데이터가 나오도록 함
                                                val rawData = chartData?.chartData
                                                    ?.filter { it.volume != "0" }  // volume이 0인 데이터는 제외 (거래가 없는 날)
                                                    ?.distinctBy { it.date }  // 중복 날짜 제거
                                                    ?.sortedBy { it.date }  // 날짜 오름차순으로 정렬(오래된 데이터가 왼쪽, 최신 데이터가 오른쪽에 오도록)
                                                
                                                Log.d("StockDetailScreen", "차트 데이터 처리: 전체 ${chartData?.chartData?.size ?: 0}개, 필터링 후 ${rawData?.size ?: 0}개")
                                                rawData?.take(5)?.forEachIndexed { idx, data ->
                                                    Log.d("StockDetailScreen", "샘플 데이터[$idx]: date=${data.date}, price=${data.currentPrice}, volume=${data.volume}")
                                                }
                                                
                                                // 인덱스 기반으로 다시 변경하되, 데이터는 정렬된 상태 유지
                                                val entries = rawData?.mapIndexed { index, data ->
                                                    CandleEntry(
                                                        index.toFloat(),
                                                        data.highPrice.toFloatOrNull()?.let { kotlin.math.abs(it) } ?: 0f,
                                                        data.lowPrice.toFloatOrNull()?.let { kotlin.math.abs(it) } ?: 0f,
                                                        data.openPrice.toFloatOrNull()?.let { kotlin.math.abs(it) } ?: 0f,
                                                        data.currentPrice.toFloatOrNull()?.let { kotlin.math.abs(it) } ?: 0f
                                                    )
                                                } ?: listOf()

                                                if (entries.isNotEmpty()) {
                                                    // Y축 최솟값 계산 - 가장 낮은 저가의 95%로 설정
                                                    val lowestLow = entries.minByOrNull { it.low }?.low ?: 0f
                                                    val yAxisMinimum = if (lowestLow > 0) lowestLow * 0.95f else 0f
                                                    
                                                    val dataSet = CandleDataSet(entries, "주가").apply {
                                                        color = AndroidColor.BLACK
                                                        shadowColorSameAsCandle = true
                                                        increasingColor = ChartBlue.toArgb()  // 상승을 파란색으로
                                                        increasingPaintStyle = Paint.Style.FILL
                                                        decreasingColor = ChartRed.toArgb()   // 하락을 빨간색으로
                                                        decreasingPaintStyle = Paint.Style.FILL
                                                        neutralColor = Gray500.toArgb()
                                                        setDrawValues(false)
                                                        highLightColor = AndroidColor.TRANSPARENT
                                                        shadowWidth = 1f
                                                        barSpace = 0.1f  // 캔들 사이 간격 조정
                                                    }

                                                    data = CandleData(dataSet)

                                                    // X축 설정
                                                    xAxis.apply {
                                                        position = XAxis.XAxisPosition.BOTTOM
                                                        setDrawGridLines(false)
                                                        labelCount = 5
                                                        textColor = Gray500.toArgb()
                                                        valueFormatter = object : ValueFormatter() {
                                                            override fun getFormattedValue(value: Float): String {
                                                                val index = value.toInt()
                                                                return if (index >= 0 && rawData != null && index < rawData.size) {
                                                                    val date = rawData[index].date
                                                                    when {
                                                                        // 분봉 (YYYYMMDDHHmm)
                                                                        selectedPeriod == "분" && date.length >= 12 -> {
                                                                            val hour = date.substring(8, 10)
                                                                            val minute = date.substring(10, 12)
                                                                            "$hour:$minute"
                                                                        }
                                                                        // 일봉 (YYYYMMDD)
                                                                        selectedPeriod == "일" && date.length >= 8 -> {
                                                                            val month = date.substring(4, 6)
                                                                            val day = date.substring(6, 8)
                                                                            "$month.$day"
                                                                        }
                                                                        // 주봉 (YYYYMMDD)
                                                                        selectedPeriod == "주" && date.length >= 8 -> {
                                                                            val month = date.substring(4, 6)
                                                                            val day = date.substring(6, 8)
                                                                            "$month.$day"
                                                                        }
                                                                        // 월봉 (YYYYMM)
                                                                        selectedPeriod == "월" && date.length >= 6 -> {
                                                                            val year = date.substring(0, 4)
                                                                            val month = date.substring(4, 6)
                                                                            "$year.$month"
                                                                        }
                                                                        // 연봉 (YYYY)
                                                                        selectedPeriod == "년" && date.length >= 4 -> {
                                                                            date.substring(0, 4)
                                                                        }
                                                                        else -> ""
                                                                    }
                                                                } else ""
                                                            }
                                                        }
                                                        granularity = 1f
                                                        setAvoidFirstLastClipping(true)
                                                    }
                                                    
                                                    // Y축 설정
                                                    axisRight.isEnabled = false
                                                    axisLeft.apply {
                                                        setDrawGridLines(true)
                                                        textColor = Gray500.toArgb()
                                                        setLabelCount(5, true)
                                                        axisMinimum = yAxisMinimum  // 계산된 Y축 최솟값 적용
                                                        setPosition(YAxis.YAxisLabelPosition.OUTSIDE_CHART)
                                                    }

                                                    // 초기 위치 설정 - 가장 최신 데이터(마지막 데이터) 표시
                                                    if (entries.isNotEmpty()) {
                                                        // 데이터의 전체 범위를 보이게 설정
                                                        fitScreen()
                                                        // 다시 오른쪽(최신 데이터) 부분으로 이동
                                                        moveViewToX(entries.size - 1f)
                                                        // 보이는 범위 설정 - 더 많은 캔들 표시
                                                        setVisibleXRangeMaximum(100f)
                                                    }
                                                    invalidate()
                                                }
                                            }
                                        },
                                        update = { chart ->
                                            // 실시간 현재가 업데이트 적용 (분/일 차트에만 적용)
                                            if (selectedPeriod == "분" || selectedPeriod == "일") {
                                                // 차트에 데이터가 있고, 웹소켓에서 현재가가 유효한 경우에만 업데이트
                                                if (chart.data != null && chart.data.dataSetCount > 0 && currentPrice > 0) {
                                                    val dataSet = chart.data.getDataSetByIndex(0) as? CandleDataSet
                                                    if (dataSet != null && dataSet.entryCount > 0) {
                                                        // 마지막 항목(최신 데이터)을 가져옴
                                                        val lastEntryIndex = dataSet.entryCount - 1
                                                        val lastEntry = dataSet.getEntryForIndex(lastEntryIndex) as? CandleEntry
                                                        if (lastEntry != null) {
                                                            // 현재가 절대값 확인
                                                            val currentPriceValue = kotlin.math.abs(currentPrice.toFloat())
                                                            
                                                            // 현재가를 반영한 고가/저가 계산
                                                            val highPrice = maxOf(lastEntry.high, currentPriceValue)
                                                            val lowPrice = minOf(lastEntry.low, currentPriceValue)
                                                            
                                                            try {
                                                                // 기존 엔트리의 값을 수정
                                                                // 시가(open)는 유지, 종가(close)만 현재가로 업데이트
                                                                lastEntry.close = currentPriceValue
                                                                
                                                                // 고가와 저가 업데이트
                                                                if (currentPriceValue > lastEntry.high) {
                                                                    lastEntry.high = currentPriceValue
                                                                }
                                                                if (currentPriceValue < lastEntry.low) {
                                                                    lastEntry.low = currentPriceValue
                                                                }
                                                                
                                                                // 데이터셋 갱신 및 차트 갱신
                                                                dataSet.notifyDataSetChanged()
                                                                chart.data.notifyDataChanged()
                                                                chart.notifyDataSetChanged()
                                                                chart.invalidate()
                                                                
                                                                Log.d("StockDetailScreen", "차트 실시간 업데이트: 현재가=$currentPriceValue, 엔트리 수=${dataSet.entryCount}, 업데이트 인덱스=$lastEntryIndex")
                                                            } catch (e: Exception) {
                                                                Log.e("StockDetailScreen", "차트 업데이트 중 오류 발생", e)
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    )
                                }
                            }
                        }
                    }
                }

                // 호가창 아이템
                item {
                    Card(
                        modifier = Modifier
                            .fillMaxWidth()
                            .padding(vertical = 16.dp),
                        colors = CardDefaults.cardColors(containerColor = Color.White),
                        border = BorderStroke(1.dp, Gray300)
                    ) {
                        if (isLoadingBidAsk) {
                            Box(
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .height(300.dp),
                                contentAlignment = Alignment.Center
                            ) {
                                CircularProgressIndicator(color = Secondary500)
                            }
                        } else {
                            if (askPrices.isNotEmpty() && bidPrices.isNotEmpty()) {
                                StockBidAskView(
                                    stockBidAsk = StockBidAsk(
                                        type = "bidask",
                                        stockCode = stock.stockCode,
                                        timestamp = stockBidAsk?.timestamp ?: apiStockBidAsk?.timestamp ?: "",
                                        askPrices = askPrices,
                                        askVolumes = askVolumes,
                                        bidPrices = bidPrices,
                                        bidVolumes = bidVolumes
                                    ),
                                    modifier = Modifier.padding(8.dp)
                                )
                            } else {
                                Text(
                                    text = "호가 데이터를 불러올 수 없습니다.",
                                    modifier = Modifier
                                        .fillMaxWidth()
                                        .padding(vertical = 24.dp),
                                    textAlign = TextAlign.Center,
                                    color = Gray500,
                                    fontFamily = SCDreamFontFamily
                                )
                            }
                    }
                }
            }

                // 체결 정보 아이템
                item {
                    Card(
                        modifier = Modifier
                            .fillMaxWidth()
                            .padding(vertical = 16.dp),
                        colors = CardDefaults.cardColors(containerColor = Color.White),
                        border = BorderStroke(1.dp, Gray300)
                    ) {
                        Column(
                            modifier = Modifier
                                .fillMaxWidth()
                                .padding(16.dp)
                        ) {
                Text(
                    text = "실시간 체결 내역",
                    fontSize = 16.sp,
                    fontWeight = FontWeight.Bold,
                    fontFamily = SCDreamFontFamily,
                                color = Gray700
                            )
                            
                            Spacer(modifier = Modifier.height(16.dp))
                            
                            if (tradeExecutions.isEmpty()) {
                                Box(
                    modifier = Modifier
                        .fillMaxWidth()
                                        .height(100.dp),
                                    contentAlignment = Alignment.Center
                                ) {
                                    Text(
                                        text = "체결 정보가 아직 없습니다",
                                        fontSize = 14.sp,
                                        color = Gray500,
                                        fontFamily = SCDreamFontFamily
                                    )
                                }
                            } else {
                                Column(
                                    modifier = Modifier.fillMaxWidth()
                ) {
                    tradeExecutions.forEach { exec ->
                        RealTimeTradeItem(exec)
                                        Spacer(modifier = Modifier.height(8.dp))
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

// 실시간 체결 내역 아이템 (마이페이지 거래 기록 스타일 참고)
@Composable
fun RealTimeTradeItem(exec: TradeExecution) {
    val isSell = exec.tradeType == "SELL"
    val time = if (exec.timestamp.length == 6) {
        "${exec.timestamp.substring(0,2)}:${exec.timestamp.substring(2,4)}:${exec.timestamp.substring(4,6)}"
    } else exec.timestamp
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .padding(vertical = 4.dp),
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.SpaceBetween
    ) {
        if (isSell) {
            // 매도(SELL) - 오른쪽 컬러 박스
            Card(
                shape = RoundedCornerShape(16.dp),
                colors = CardDefaults.cardColors(containerColor = Color.Transparent),
                elevation = CardDefaults.cardElevation(defaultElevation = 0.dp),
                modifier = Modifier.weight(1f).padding(end = 8.dp)
            ) {
                Row(
                    modifier = Modifier
                        .fillMaxWidth()
                        .height(40.dp),
                    verticalAlignment = Alignment.CenterVertically
                ) {
                    Row(
                        modifier = Modifier
                            .weight(1f)
                            .background(Color(0xFFF5F5F5))
                            .fillMaxHeight()
                            .padding(horizontal = 12.dp),
                        verticalAlignment = Alignment.CenterVertically
                    ) {
                        Text(
                            text = "${exec.price.toFormattedString()}원",
                            fontSize = 11.sp,
                            fontWeight = FontWeight.W700,
                            fontFamily = SCDreamFontFamily
                        )
                        Spacer(modifier = Modifier.width(8.dp))
                        Text(
                            text = "${exec.quantity}주",
                            fontSize = 11.sp,
                            fontWeight = FontWeight.W700,
                            fontFamily = SCDreamFontFamily
                        )
                    }
                    Box(
                        modifier = Modifier
                            .background(Color(0xFF4DABF7))  // 매도를 빨간색으로
                            .fillMaxHeight()
                            .padding(horizontal = 12.dp)
                            .clip(RoundedCornerShape(topEnd = 16.dp, bottomEnd = 16.dp)),
                        contentAlignment = Alignment.Center
                    ) {
                        Text(
                            text = "매도",
                            fontSize = 10.sp,
                            color = Color.White,
                            fontFamily = SCDreamFontFamily,
                            fontWeight = FontWeight.Bold
                        )
                    }
                }
            }
            Column(horizontalAlignment = Alignment.End, modifier = Modifier.width(70.dp)) {
                Text(text = time, fontSize = 10.sp, color = Color.Gray, fontFamily = SCDreamFontFamily)
            }
        } else {
            // 매수(BUY) - 왼쪽 컬러 박스
            Column(horizontalAlignment = Alignment.Start, modifier = Modifier.width(70.dp)) {
                Text(text = time, fontSize = 10.sp, color = Color.Gray, fontFamily = SCDreamFontFamily)
            }
            Card(
                shape = RoundedCornerShape(16.dp),
                colors = CardDefaults.cardColors(containerColor = Color.Transparent),
                elevation = CardDefaults.cardElevation(defaultElevation = 0.dp),
                modifier = Modifier.weight(1f).padding(start = 8.dp)
            ) {
                Row(
                    modifier = Modifier
                        .fillMaxWidth()
                        .height(40.dp),
                    verticalAlignment = Alignment.CenterVertically
                ) {
                    Box(
                        modifier = Modifier
                            .background(Color(0xFFFF6B6B))  // 매수를 파란색으로
                            .height(40.dp)
                            .padding(horizontal = 12.dp)
                            .clip(RoundedCornerShape(topStart = 16.dp, bottomStart = 16.dp)),
                        contentAlignment = Alignment.Center
                    ) {
                        Text(
                            text = "매수",  // 매수로 수정
                            fontSize = 10.sp,
                            color = Color.White,
                            fontFamily = SCDreamFontFamily,
                            fontWeight = FontWeight.Bold
                        )
                    }
                    Row(
                        modifier = Modifier
                            .weight(1f)
                            .background(Color(0xFFF5F5F5))
                            .height(40.dp)
                            .padding(horizontal = 12.dp),
                        verticalAlignment = Alignment.CenterVertically,
                        horizontalArrangement = Arrangement.End
                    ) {
                        Text(
                            text = "${exec.price.toFormattedString()}원",
                            fontSize = 11.sp,
                            fontWeight = FontWeight.W700,
                            fontFamily = SCDreamFontFamily
                        )
                        Spacer(modifier = Modifier.width(8.dp))
                        Text(
                            text = "${exec.quantity}주",
                            fontSize = 11.sp,
                            fontWeight = FontWeight.W700,
                            fontFamily = SCDreamFontFamily
                        )
                    }
                }
            }
        }
    }
}

fun Int.toFormattedString(): String = String.format("%,d", this)

// 관심 종목 여부 체크 함수 (공통)
fun isInWatchlist(watchlist: Set<String>, stockCode: String): Boolean {
    return watchlist.any { it.trim().equals(stockCode.trim(), ignoreCase = true) }
}

// 종목 코드에서 _AL 등 접미사를 제거하는 함수
fun getPureStockCode(stockCode: String): String {
    return stockCode.substringBefore("_")
}

