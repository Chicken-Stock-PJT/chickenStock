package com.example.chickenstock.ui.screens.stock

import android.graphics.Color as AndroidColor
import androidx.compose.foundation.layout.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.KeyboardArrowLeft
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
    var selectedPeriod by remember { mutableStateOf("일") }
    var showMinuteDropdown by remember { mutableStateOf(false) }
    val minuteOptions = listOf("1분", "3분", "5분", "10분", "30분", "60분")
    val coroutineScope = rememberCoroutineScope()

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

    // 현재 잔고 계산
    val currentBalance = userProfile?.memberMoney?.replace(",", "")?.toIntOrNull() ?: 0

    // 임시 보유 주식 데이터
    val holdingQuantity = 100
    val averagePrice = 55000
    val currentPrice = stock.currentPrice.toInt()
    val profitRate = ((currentPrice.toFloat() - averagePrice.toFloat()) / averagePrice.toFloat() * 100)
    val totalProfit = (currentPrice.toLong() - averagePrice.toLong()) * holdingQuantity.toLong()

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

    // 현재 보유 주식 정보 찾기
    val currentPosition = remember(stock.stockCode) {
        viewModel.portfolioData.value?.positions?.find { position -> 
            position.stockCode == stock.stockCode 
        }
    }

    // 웹소켓 연결
    LaunchedEffect(stock.stockCode) {
        Log.d("StockDetailScreen", "웹소켓 연결 시작: ${stock.stockCode}")
        webSocketManager.connect(stock.stockCode)
    }

    // 화면이 사라질 때 웹소켓 연결 해제
    DisposableEffect(Unit) {
        onDispose {
            Log.d("StockDetailScreen", "웹소켓 연결 해제")
            webSocketManager.disconnect()
        }
    }

    // 상단 가격 정보 업데이트
    LaunchedEffect(stockPrice) {
        stockPrice?.let { price ->
            Log.d("StockDetailScreen", "주식 가격 업데이트: ${price.currentPrice}")
        }
    }

    // 호가 데이터 업데이트
    LaunchedEffect(stockBidAsk) {
        stockBidAsk?.let { bidAsk ->
            Log.d("StockDetailScreen", "호가 데이터 업데이트: ${bidAsk.timestamp}")
        }
    }

    // 종목 상세 정보 로드
    LaunchedEffect(stock.stockCode) {
        try {
            val stockService = RetrofitClient.getInstance(context).create(StockService::class.java)
            val response = stockService.getStockDetail(stock.stockCode)
            if (response.isSuccessful) {
                stockDetail = response.body()
                isLoading = false
            }
        } catch (e: Exception) {
            Log.e("StockDetailScreen", "Failed to load stock detail", e)
            isLoading = false
        }
    }

    // 차트 데이터 로드
    LaunchedEffect(selectedPeriod) {
        isChartLoading = true
        try {
            val chartType = when (selectedPeriod) {
                "1분", "3분", "5분", "10분", "30분", "60분" -> "MINUTE"
                "일" -> "DAILY"
                "주" -> "WEEKLY"
                "월" -> "MONTHLY"
                "년" -> "YEARLY"
                else -> "DAILY"
            }
            
            val timeInterval = if (selectedPeriod.endsWith("분")) {
                selectedPeriod.replace("분", "")
            } else null

            Log.d("StockDetailScreen", "차트 데이터 요청: chartType=$chartType, timeInterval=$timeInterval, stockCode=${stock.stockCode}")

            val stockService = RetrofitClient.getInstance(context).create(StockService::class.java)
            val response = stockService.getStockChart(
                chartType = chartType,
                stockCode = stock.stockCode,
                timeInterval = timeInterval,
                nextKey = null
            )
            chartData = response
            nextKey = response.nextKey
            hasNext = response.hasNext
            Log.d("StockDetailScreen", "차트 데이터 로드 성공: ${response.chartData.size}개, nextKey=${response.nextKey}, hasNext=${response.hasNext}")
            
            // 데이터 상세 로그
            response.chartData.take(3).forEachIndexed { index, data ->
                Log.d("StockDetailScreen", "데이터 $index: date=${data.date}, currentPrice=${data.currentPrice}, openPrice=${data.openPrice}, highPrice=${data.highPrice}, lowPrice=${data.lowPrice}")
            }
        } catch (e: Exception) {
            Log.e("StockDetailScreen", "차트 데이터 로드 실패", e)
            Toast.makeText(context, "차트 데이터를 불러오는데 실패했습니다.", Toast.LENGTH_SHORT).show()
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
                        "1분", "3분", "5분", "10분", "30분", "60분" -> "MINUTE"
                        "일" -> "DAILY"
                        "주" -> "WEEKLY"
                        "월" -> "MONTHLY"
                        "년" -> "YEARLY"
                        else -> "DAILY"
                    }
                    
                    val timeInterval = if (selectedPeriod.endsWith("분")) {
                        selectedPeriod.replace("분", "")
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
    var loginDialogType by remember { mutableStateOf("") } // "buy" 또는 "sell"

    // 프로필 정보 로드
    LaunchedEffect(authViewModel.isLoggedIn.value) {
        if (authViewModel.isLoggedIn.value) {
            isProfileLoading = true
            try {
                val memberService = RetrofitClient.getInstance(context).create(MemberService::class.java)
                val response = memberService.getSimpleProfile()
                if (response.isSuccessful) {
                    userProfile = response.body()
                }
            } catch (e: Exception) {
                profileError = e.message
            } finally {
                isProfileLoading = false
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
                    if (loginDialogType == "buy") "매수 주문을 위해서는 로그인이 필요합니다."
                    else "매도 주문을 위해서는 로그인이 필요합니다.",
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

    if (showSellBottomSheet) {
        val scrollState = rememberScrollState()

        // 매도 바텀 시트 초기화
        LaunchedEffect(Unit) {
            sellPrice = stockPrice?.currentPrice.replace("""[+\-]""".toRegex(), "") ?: stock.currentPrice.replace("""[+\-]""".toRegex(), "")
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
                            Text(
                                text = "매도 주문",
                                fontSize = 20.sp,
                                fontWeight = FontWeight.Bold,
                                fontFamily = SCDreamFontFamily,
                                color = Gray700
                            )
                            
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
                            
                            // 보유 주식이 있을 때만 추가 정보 표시
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
                            
                            // 주문 유형 선택 (주석 처리)
                            /*
                            Row(
                                modifier = Modifier.fillMaxWidth(),
                                horizontalArrangement = Arrangement.spacedBy(8.dp)
                            ) {
                                listOf("지정가", "시장가").forEach { type ->
                                    Button(
                                        onClick = { 
                                            sellOrderType = type
                                            if (type == "시장가") {
                                                sellPrice = stock.currentPrice.toString()
                                            }
                                        },
                                        colors = ButtonDefaults.buttonColors(
                                            containerColor = if (sellOrderType == type) ChartBlue else Gray200,
                                            contentColor = if (sellOrderType == type) Color.White else Gray700
                                        ),
                                        modifier = Modifier.weight(1f)
                                    ) {
                                        Text(
                                            text = type,
                                            fontSize = 14.sp,
                                            fontFamily = SCDreamFontFamily
                                        )
                                    }
                                }
                            }

                            Spacer(modifier = Modifier.height(24.dp))
                            */
                            
                            // 가격 입력 (현재가로 자동 설정, 수정 불가)
                            OutlinedTextField(
                                value = sellPrice,
                                onValueChange = { },
                                label = { Text("주문 가격", fontFamily = SCDreamFontFamily) },
                                enabled = false,
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
                            
                            // 수량 입력 (매도)
                            Row(
                                modifier = Modifier.fillMaxWidth(),
                                horizontalArrangement = Arrangement.spacedBy(8.dp),
                                verticalAlignment = Alignment.CenterVertically
                            ) {
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
                                            val currentHolding = currentPosition?.quantity ?: 0
                                            if (sellPrice.isNotEmpty() && sellQuantity.isNotEmpty() && quantity <= currentHolding) {
                                                hideKeyboard()
                                                sellStep = 2
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
                                
                                Button(
                                    onClick = { 
                                        sellQuantity = (currentPosition?.quantity ?: 0).toString()
                                    },
                                    colors = ButtonDefaults.buttonColors(
                                        containerColor = ChartBlue
                                    ),
                                    modifier = Modifier.height(56.dp).offset(y = 6.dp)
                                ) {
                                    Text(
                                        text = "최대",
                                        fontSize = 14.sp,
                                        fontWeight = FontWeight.Bold,
                                        fontFamily = SCDreamFontFamily,
                                        color = Color.White
                                    )
                                }
                            }

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
                            
                            Spacer(modifier = Modifier.height(16.dp))
                            
                            // 총 매도 금액
                            val totalAmount = try {
                                val price = sellPrice.toIntOrNull() ?: 0
                                val sellQty = sellQuantity.toIntOrNull() ?: 0
                                price * sellQty
                            } catch (e: Exception) { 0 }
                            
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
                            
                            // 매도 버튼
                            Button(
                                onClick = { 
                                    hideKeyboard()
                                    sellStep = 2 
                                },
                                enabled = sellPrice.isNotEmpty() && 
                                         sellQuantity.isNotEmpty() && 
                                         (sellQuantity.toIntOrNull() ?: 0) <= (currentPosition?.quantity ?: 0) &&
                                         (sellQuantity.toIntOrNull() ?: 0) > 0,
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .height(48.dp),
                                colors = ButtonDefaults.buttonColors(
                                    containerColor = ChartBlue,
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
                        // 매도 시트 2번째 화면
                        Text(
                            text = "매도 주문 확인",
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
                                    color = ChartBlue,
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
                                OrderInfoRow("주문 유형", sellOrderType)
                                OrderInfoRow("주문 가격", "${String.format("%,d", sellPrice.toIntOrNull() ?: 0)}원")
                                OrderInfoRow("주문 수량", "${sellQuantity}주")
                                OrderInfoRow("총 매도 금액", "${String.format("%,d", (sellPrice.toIntOrNull() ?: 0) * (sellQuantity.toIntOrNull() ?: 0))}원")
                                Divider(modifier = Modifier.padding(vertical = 8.dp), color = Gray200)
                                OrderInfoRow("현재 보유 수량", "${currentPosition?.quantity ?: 0}주")
                                OrderInfoRow(
                                    "매도 후 보유 수량", 
                                    "${(currentPosition?.quantity ?: 0) - (sellQuantity.toIntOrNull() ?: 0)}주"
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
                                        showSellBottomSheet = false
                                        sellStep = 1
                                        sellOrderType = "시장가"
                                        sellPrice = ""
                                        sellQuantity = ""
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
                                            val response = stockService.sellStock(
                                                SellOrderRequest(
                                                    stockCode = stock.stockCode,
                                                    quantity = sellQuantity.toInt(),
                                                    price = null,
                                                    marketOrder = true
                                                )
                                            )
                                            
                                            if (response.isSuccessful) {
                                                delay(500) // 성공 시 잠시 대기
                                                isLoading = false
                                                sellStep = 3
                                                showSellSuccessMessage = true
                                                
                                                // 프로필 정보 새로고침 (잔고 업데이트를 위해)
                                                val memberService = RetrofitClient.getInstance(context).create(MemberService::class.java)
                                                val profileResponse = memberService.getSimpleProfile()
                                                if (profileResponse.isSuccessful) {
                                                    userProfile = profileResponse.body()
                                                }
                                            } else {
                                                // 실패 시 에러 메시지 표시
                                                Toast.makeText(
                                                    context,
                                                    "주문 실패: ${response.message()}",
                                                    Toast.LENGTH_SHORT
                                                ).show()
                                                isLoading = false
                                            }
                                        } catch (e: Exception) {
                                            // 에러 발생 시
                                            Toast.makeText(
                                                context,
                                                "주문 실패: ${e.message}",
                                                Toast.LENGTH_SHORT
                                            ).show()
                                            isLoading = false
                                        }
                                    }
                                },
                                colors = ButtonDefaults.buttonColors(
                                    containerColor = ChartBlue
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
            buyPrice = stockPrice?.currentPrice.replace("""[+\-]""".toRegex(), "") ?: stock.currentPrice.replace("""[+\-]""".toRegex(), "")
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
                            Text(
                                text = "매수 주문",
                                fontSize = 20.sp,
                                fontWeight = FontWeight.Bold,
                                fontFamily = SCDreamFontFamily,
                                color = Gray700
                            )
                            
                            Spacer(modifier = Modifier.height(16.dp))

                            // 현재 잔고 표시 부분 수정
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
                                    text = "${String.format("%,d", userProfile?.memberMoney?.replace(",", "")?.toIntOrNull() ?: 0)}원",
                                    fontSize = 16.sp,
                                    fontWeight = FontWeight.Bold,
                                    color = Gray700,
                                    fontFamily = SCDreamFontFamily
                                )
                            }
                            
                            Spacer(modifier = Modifier.height(24.dp))
                            
                            // 주문 유형 선택 (주석 처리)
                            /*
                            Row(
                                modifier = Modifier.fillMaxWidth(),
                                horizontalArrangement = Arrangement.spacedBy(8.dp)
                            ) {
                                listOf("지정가", "시장가").forEach { type ->
                                    Button(
                                        onClick = { 
                                            buyOrderType = type
                                            if (type == "시장가") {
                                                buyPrice = stock.currentPrice.toString()
                                            }
                                        },
                                        colors = ButtonDefaults.buttonColors(
                                            containerColor = if (buyOrderType == type) Secondary500 else Gray200,
                                            contentColor = if (buyOrderType == type) Color.White else Gray700
                                        ),
                                        modifier = Modifier.weight(1f)
                                    ) {
                                        Text(
                                            text = type,
                                            fontSize = 14.sp,
                                            fontFamily = SCDreamFontFamily
                                        )
                                    }
                                }
                            }

                            Spacer(modifier = Modifier.height(24.dp))
                            */
                            
                            // 가격 입력 (현재가로 자동 설정, 수정 불가)
                            OutlinedTextField(
                                value = buyPrice,
                                onValueChange = { },
                                label = { Text("주문 가격", fontFamily = SCDreamFontFamily) },
                                enabled = false,
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
                                    focusedBorderColor = Secondary500,
                                    unfocusedBorderColor = Gray300,
                                    focusedTextColor = Color.Black,
                                    unfocusedTextColor = Color.Black,
                                    disabledTextColor = Color.Black,
                                    disabledBorderColor = Gray300,
                                    disabledLabelColor = Gray700
                                )
                            )
                            
                            Spacer(modifier = Modifier.height(8.dp))
                            
                            // 수량 입력 (매수)
                            Row(
                                modifier = Modifier.fillMaxWidth(),
                                horizontalArrangement = Arrangement.spacedBy(8.dp),
                                verticalAlignment = Alignment.CenterVertically
                            ) {
                                OutlinedTextField(
                                    value = buyQuantity,
                                    onValueChange = { buyQuantity = it },
                                    label = { Text("주문 수량", fontFamily = SCDreamFontFamily) },
                                    keyboardOptions = KeyboardOptions(
                                        keyboardType = KeyboardType.Number,
                                        imeAction = ImeAction.Done
                                    ),
                                    keyboardActions = KeyboardActions(
                                        onDone = {
                                            val totalAmount = try {
                                                val price = buyPrice.toIntOrNull() ?: 0
                                                val quantity = buyQuantity.toIntOrNull() ?: 0
                                                price * quantity
                                            } catch (e: Exception) { 0 }

                                            if (buyPrice.isNotEmpty() && buyQuantity.isNotEmpty() && totalAmount <= currentBalance) {
                                                hideKeyboard()
                                                buyStep = 2
                                            }
                                        }
                                    ),
                                    modifier = Modifier
                                        .weight(1f)
                                        .focusRequester(quantityFocusRequester),
                                    colors = OutlinedTextFieldDefaults.colors(
                                        focusedBorderColor = Secondary500,
                                        unfocusedBorderColor = Gray300,
                                        focusedTextColor = Color.Black,
                                        unfocusedTextColor = Color.Black
                                    )
                                )
                                
                                Button(
                                    onClick = { 
                                        val price = buyPrice.toIntOrNull() ?: 0
                                        if (price > 0) {
                                            val balance = userProfile?.memberMoney?.replace(",", "")?.toIntOrNull() ?: 0
                                            val maxQuantity = balance / price
                                            buyQuantity = maxQuantity.toString()
                                        }
                                    },
                                    colors = ButtonDefaults.buttonColors(
                                        containerColor = Secondary500
                                    ),
                                    modifier = Modifier.height(56.dp).offset(y = 6.dp)
                                ) {
                                    Text(
                                        text = "최대",
                                        fontSize = 14.sp,
                                        fontWeight = FontWeight.Bold,
                                        fontFamily = SCDreamFontFamily,
                                        color = Color.White
                                    )
                                }
                            }
                            
                            Spacer(modifier = Modifier.height(16.dp))
                            
                            // 총 주문 금액
                            val totalAmount = try {
                                val price = buyPrice.toIntOrNull() ?: 0
                                val quantity = buyQuantity.toIntOrNull() ?: 0
                                price * quantity
                            } catch (e: Exception) { 0 }
                            
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
                            
                            // 매수 버튼
                            Button(
                                onClick = { 
                                    hideKeyboard()
                                    buyStep = 2 
                                },
                                enabled = buyPrice.isNotEmpty() && buyQuantity.isNotEmpty() && totalAmount <= currentBalance,
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .height(48.dp),
                                colors = ButtonDefaults.buttonColors(
                                    containerColor = Secondary500,
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
                        // 매수 시트 2번째 화면
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
                                    color = Secondary500,
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
                                OrderInfoRow("총 주문 금액", "${String.format("%,d", (buyPrice.toIntOrNull() ?: 0) * (buyQuantity.toIntOrNull() ?: 0))}원")
                                Divider(modifier = Modifier.padding(vertical = 8.dp), color = Gray200)
                                OrderInfoRow("현재 잔고", "${String.format("%,d", currentBalance)}원")
                                OrderInfoRow("주문 후 잔고", "${String.format("%,d", currentBalance - ((buyPrice.toIntOrNull() ?: 0) * (buyQuantity.toIntOrNull() ?: 0)))}원")
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
                                            val response = stockService.buyStock(
                                                BuyOrderRequest(
                                                    stockCode = stock.stockCode,
                                                    quantity = buyQuantity.toInt(),
                                                    price = null,
                                                    marketOrder = true
                                                )
                                            )
                                            
                                            if (response.isSuccessful) {
                                                delay(500) // 성공 시 잠시 대기
                                                isLoading = false
                                                buyStep = 3
                                                showBuySuccessMessage = true
                                                
                                                // 프로필 정보 새로고침 (잔고 업데이트를 위해)
                                                val memberService = RetrofitClient.getInstance(context).create(MemberService::class.java)
                                                val profileResponse = memberService.getSimpleProfile()
                                                if (profileResponse.isSuccessful) {
                                                    userProfile = profileResponse.body()
                                                }
                                            } else {
                                                // 실패 시 에러 메시지 표시
                                                Toast.makeText(
                                                    context,
                                                    "주문 실패: ${response.message()}",
                                                    Toast.LENGTH_SHORT
                                                ).show()
                                                isLoading = false
                                            }
                                        } catch (e: Exception) {
                                            // 에러 발생 시
                                            Toast.makeText(
                                                context,
                                                "주문 실패: ${e.message}",
                                                Toast.LENGTH_SHORT
                                            ).show()
                                            isLoading = false
                                        }
                                    }
                                },
                                colors = ButtonDefaults.buttonColors(
                                    containerColor = ChartBlue
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
                    3 -> {
                        // 주문 완료 화면
                        Column(
                            modifier = Modifier.fillMaxWidth(),
                            horizontalAlignment = Alignment.CenterHorizontally
                        ) {
                            Icon(
                                imageVector = Icons.Default.CheckCircle,
                                contentDescription = "성공",
                                tint = Secondary500,
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
                                    showBuyBottomSheet = false
                                    buyStep = 1
                                    buyOrderType = "시장가"
                                    buyPrice = ""
                                    buyQuantity = ""
                                    showBuySuccessMessage = false
                                },
                                colors = ButtonDefaults.buttonColors(
                                    containerColor = Secondary500
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

    Scaffold(
        containerColor = Gray0,
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
                    IconButton(
                        onClick = { /* 즐겨찾기 */ },
                        modifier = Modifier.size(72.dp)
                    ) {
                        Icon(
                            imageVector = Icons.Outlined.FavoriteBorder,
                            contentDescription = "Favorite",
                            tint = Gray300,
                            modifier = Modifier.size(48.dp)
                        )
                    }
                },
                colors = TopAppBarDefaults.topAppBarColors(
                    containerColor = Gray0
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
                // 매도 버튼 (왼쪽)
                Button(
                    onClick = { 
                        if (authViewModel.isLoggedIn.value) {
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
                        containerColor = ChartBlue
                    ),
                    shape = RoundedCornerShape(8.dp)
                ) {
                    Text(
                        text = "매도",
                        fontSize = 14.sp,
                        fontWeight = FontWeight.W700,
                        fontFamily = SCDreamFontFamily,
                        color = Color.White
                    )
                }

                Spacer(modifier = Modifier.width(12.dp))

                // 매수 버튼 (오른쪽)
                Button(
                    onClick = { 
                        if (authViewModel.isLoggedIn.value) {
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
                        containerColor = Secondary500
                    ),
                    shape = RoundedCornerShape(8.dp)
                ) {
                    Text(
                        text = "매수",
                        fontSize = 14.sp,
                        fontWeight = FontWeight.Bold,
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
                .padding(paddingValues)
        ) {
            // 상단 고정 부분 (종목 이름, 가격 등)
            Row(
                verticalAlignment = Alignment.Top,
                modifier = Modifier.padding(start = 32.dp, top = 20.dp, end = 32.dp, bottom = 10.dp)
            ) {
                AsyncImage(
                    model = "https://thumb.tossinvest.com/image/resized/96x0/https%3A%2F%2Fstatic.toss.im%2Fpng-icons%2Fsecurities%2Ficn-sec-fill-${stock.stockCode}.png",
                    contentDescription = "주식 로고",
                    modifier = Modifier
                        .size(46.dp)
                        .clip(RoundedCornerShape(12.dp))
                        .background(Color.White)
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
                        Text(
                            text = "${stockPrice?.currentPrice.replace("""[+\-]""".toRegex(), "") ?: stock.currentPrice.replace("""[+\-]""".toRegex(), "")}원",
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
                        val currentPriceValue = stockPrice?.currentPrice?.replace("""[+\-,]""".toRegex(), "")?.toIntOrNull() 
                            ?: stock.currentPrice.replace("""[+\-,]""".toRegex(), "").toIntOrNull() 
                            ?: 0
                        val fluctuationRate = stockPrice?.changeRate?.replace("""[+\-,%]""".toRegex(), "")?.toFloatOrNull()
                            ?: stock.fluctuationRate.replace("""[+\-,%]""".toRegex(), "").toFloatOrNull()
                            ?: 0f
                        val priceChange = (currentPriceValue * fluctuationRate / 100).toInt()
                        val isPositive = stockPrice?.let { it.isPositiveChange() }
                            ?: stock.fluctuationRate.startsWith("+")

                        Text(
                            text = "${if (isPositive) "+" else "-"}${String.format("%,d", Math.abs(priceChange))}원",
                            fontSize = 12.sp,
                            fontWeight = FontWeight.W500,
                            color = if (isPositive) Color.Red else Color.Blue,
                            fontFamily = SCDreamFontFamily
                        )
                        Text(
                            text = " (${stockPrice?.changeRate ?: stock.fluctuationRate}%)",
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
                item {
                    // 차트 부분
                    Card(
                        modifier = Modifier
                            .fillMaxWidth()
                            .height(400.dp)
                            .padding(vertical = 16.dp),
                        colors = CardDefaults.cardColors(containerColor = Gray50)
                    ) {
                        Column(
                            modifier = Modifier
                                .fillMaxSize()
                                .padding(top = 16.dp)
                        ) {
                            // 기간 선택 버튼들
                            Row(
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .padding(bottom = 8.dp),
                                horizontalArrangement = Arrangement.End
                            ) {
                                // 분 버튼 (드롭다운)
                                Box {
                                    Row(
                                        modifier = Modifier
                                            .clip(RoundedCornerShape(8.dp))
                                            .background(if (selectedPeriod.endsWith("분")) Gray100 else Color.Transparent)
                                            .clickable { showMinuteDropdown = true }
                                            .padding(horizontal = 12.dp, vertical = 6.dp),
                                        verticalAlignment = Alignment.CenterVertically
                                    ) {
                                        Text(
                                            text = selectedPeriod.takeIf { it.endsWith("분") } ?: "1분",
                                            fontSize = 12.sp,
                                            color = Gray700,
                                            fontFamily = SCDreamFontFamily
                                        )
                                        Icon(
                                            imageVector = Icons.Default.KeyboardArrowDown,
                                            contentDescription = "드롭다운",
                                            tint = Gray700,
                                            modifier = Modifier.size(16.dp)
                                        )
                                    }
                                    DropdownMenu(
                                        expanded = showMinuteDropdown,
                                        onDismissRequest = { showMinuteDropdown = false },
                                        modifier = Modifier.background(Gray0)
                                    ) {
                                        minuteOptions.forEach { option ->
                                            DropdownMenuItem(
                                                text = { 
                                                    Text(
                                                        text = option, 
                                                        fontSize = 12.sp, 
                                                        fontFamily = SCDreamFontFamily,
                                                        color = Gray700
                                                    ) 
                                                },
                                                onClick = {
                                                    selectedPeriod = option
                                                    showMinuteDropdown = false
                                                }
                                            )
                                        }
                                    }
                                }
                                
                                Spacer(modifier = Modifier.width(8.dp))

                                // 일/주/월/년 버튼들
                                listOf("일", "주", "월", "년").forEach { period ->
                                    Text(
                                        text = period,
                                        fontSize = 12.sp,
                                        color = Gray700,
                                        modifier = Modifier
                                            .clip(RoundedCornerShape(8.dp))
                                            .background(if (selectedPeriod == period) Gray200 else Color.Transparent)
                                            .clickable { selectedPeriod = period }
                                            .padding(horizontal = 12.dp, vertical = 6.dp),
                                        fontFamily = SCDreamFontFamily
                                    )
                                    Spacer(modifier = Modifier.width(8.dp))
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
                                                
                                                // 차트 표시 설정
                                                setVisibleXRangeMaximum(30f)  // 한 번에 보이는 캔들 개수
                                                setVisibleXRangeMinimum(10f)  // 최소 보이는 캔들 개수
                                                
                                                // 차트 여백 설정
                                                setViewPortOffsets(40f, 20f, 40f, 20f)
                                                
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

                                                val entries = chartData?.chartData
                                                    ?.sortedBy { it.date }  // 날짜순으로 정렬
                                                    ?.takeLast(100)
                                                    ?.mapIndexed { index, data ->
                                                        CandleEntry(
                                                            index.toFloat(),
                                                            data.highPrice.toFloatOrNull() ?: 0f,
                                                            data.lowPrice.toFloatOrNull() ?: 0f,
                                                            data.openPrice.toFloatOrNull() ?: 0f,
                                                            data.currentPrice.toFloatOrNull() ?: 0f
                                                        )
                                                    } ?: listOf()

                                                if (entries.isNotEmpty()) {
                                                    val dataSet = CandleDataSet(entries, "주가").apply {
                                                        color = AndroidColor.BLACK
                                                        shadowColorSameAsCandle = true
                                                        increasingColor = ChartRed.toArgb()
                                                        increasingPaintStyle = Paint.Style.FILL
                                                        decreasingColor = ChartBlue.toArgb()
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
                                                                return if (index >= 0 && index < entries.size) {
                                                                    val date = chartData?.chartData?.get(index)?.date ?: ""
                                                                    when {
                                                                        // 분봉 (YYYYMMDDHHmm)
                                                                        selectedPeriod.endsWith("분") && date.length >= 12 -> {
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
                                                        setPosition(YAxis.YAxisLabelPosition.OUTSIDE_CHART)
                                                    }

                                                    // 초기 위치 설정 - 가장 최근 데이터 표시
                                                    moveViewToX(entries.size.toFloat())
                                                    setVisibleXRangeMaximum(50f)  // 한 번에 보이는 캔들 개수 조정
                                                    invalidate()
                                                }
                                            }
                                        }
                                    )
                                }
                            }
                        }
                    }

                    // 호가창 추가
                    Card(
                        modifier = Modifier
                            .fillMaxWidth()
                            .padding(vertical = 16.dp),
                        colors = CardDefaults.cardColors(containerColor = Gray50)
                    ) {
                        stockBidAsk?.let { bidAsk ->
                            StockBidAskView(
                                stockBidAsk = bidAsk,
                                modifier = Modifier.padding(8.dp)
                            )
                        }
                    }
                }
            }
        }
    }
}
