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

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun StockDetailScreen(
    navController: NavController,
    stock: StockItem
) {
    val context = LocalContext.current
    var selectedPeriod by remember { mutableStateOf("주") }
    var showMinuteDropdown by remember { mutableStateOf(false) }
    val minuteOptions = listOf("1분", "3분", "5분", "10분", "30분", "60분")

    var showSellBottomSheet by remember { mutableStateOf(false) }
    var showBuyBottomSheet by remember { mutableStateOf(false) }
    var isShowingCandleChart by remember { mutableStateOf(false) }
    var isLoading by remember { mutableStateOf(false) }
    val bottomSheetState = rememberModalBottomSheetState(
        skipPartiallyExpanded = true,
        confirmValueChange = { true }
    )

    // 매수 관련 상태들
    var buyOrderType by remember { mutableStateOf("지정가") }
    var buyPrice by remember { mutableStateOf("") }
    var buyQuantity by remember { mutableStateOf("") }
    var buyStep by remember { mutableStateOf(1) }
    var showSuccessMessage by remember { mutableStateOf(false) }
    
    // 매도 관련 상태들
    var sellOrderType by remember { mutableStateOf("지정가") }
    var sellPrice by remember { mutableStateOf("") }
    var sellQuantity by remember { mutableStateOf("") }
    var sellStep by remember { mutableStateOf(1) }
    var showSellSuccessMessage by remember { mutableStateOf(false) }
    
    val currentBalance = 1000000 // 임시 잔고 데이터

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

    if (showSellBottomSheet) {
        val scrollState = rememberScrollState()

        ModalBottomSheet(
            onDismissRequest = { 
                hideKeyboard()
                showSellBottomSheet = false
                sellStep = 1
                sellOrderType = "지정가"
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
                                text = "${holdingQuantity}주",
                                fontSize = 16.sp,
                                fontWeight = FontWeight.Bold,
                                color = Gray700,
                                fontFamily = SCDreamFontFamily
                            )
                            
                            Spacer(modifier = Modifier.height(12.dp))
                            
                            Text(
                                text = "평균 매수가",
                                fontSize = 14.sp,
                                color = Gray500,
                                fontFamily = SCDreamFontFamily
                            )
                            Text(
                                text = "${String.format("%,d", averagePrice)}원",
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
                                text = String.format("%.2f%%", profitRate),
                                fontSize = 16.sp,
                                fontWeight = FontWeight.Bold,
                                color = when {
                                    profitRate > 0f -> ChartRed
                                    profitRate < 0f -> ChartBlue
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
                                text = "${String.format("%,d", totalProfit)}원",
                                fontSize = 16.sp,
                                fontWeight = FontWeight.Bold,
                                color = when {
                                    totalProfit > 0L -> ChartRed
                                    totalProfit < 0L -> ChartBlue
                                    else -> Gray700
                                },
                                fontFamily = SCDreamFontFamily
                            )
                            
                            Spacer(modifier = Modifier.height(24.dp))
                            
                            // 주문 유형 선택
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
                            
                            // 가격 입력
                            OutlinedTextField(
                                value = sellPrice,
                                onValueChange = { if (sellOrderType == "지정가") sellPrice = it },
                                label = { Text("주문 가격", fontFamily = SCDreamFontFamily) },
                                enabled = sellOrderType == "지정가",
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
                                    unfocusedBorderColor = Gray300
                                )
                            )
                            
                            Spacer(modifier = Modifier.height(8.dp))
                            
                            // 수량 입력
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
                                        if (sellPrice.isNotEmpty() && sellQuantity.isNotEmpty() && quantity <= holdingQuantity) {
                                            hideKeyboard()
                                            sellStep = 2
                                        }
                                    }
                                ),
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .focusRequester(quantityFocusRequester),
                                colors = OutlinedTextFieldDefaults.colors(
                                    focusedBorderColor = ChartBlue,
                                    unfocusedBorderColor = Gray300
                                )
                            )
                            
                            Spacer(modifier = Modifier.height(16.dp))
                            
                            // 총 매도 금액
                            val totalAmount = try {
                                val price = sellPrice.toIntOrNull() ?: 0
                                val quantity = sellQuantity.toIntOrNull() ?: 0
                                price * quantity
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
                                enabled = sellPrice.isNotEmpty() && sellQuantity.isNotEmpty() && 
                                         (sellQuantity.toIntOrNull() ?: 0) <= holdingQuantity,
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
                                    fontFamily = SCDreamFontFamily
                                )
                            }
                        }
                    }
                    2 -> {
                        // 주문 확인 화면
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
                                OrderInfoRow("종목명", stock.stockName)
                                OrderInfoRow("주문 유형", sellOrderType)
                                OrderInfoRow("주문 가격", "${String.format("%,d", sellPrice.toIntOrNull() ?: 0)}원")
                                OrderInfoRow("주문 수량", "${sellQuantity}주")
                                OrderInfoRow("총 매도 금액", "${String.format("%,d", (sellPrice.toIntOrNull() ?: 0) * (sellQuantity.toIntOrNull() ?: 0))}원")
                                Divider(modifier = Modifier.padding(vertical = 8.dp), color = Gray200)
                                OrderInfoRow("현재 보유 수량", "${holdingQuantity}주")
                                OrderInfoRow(
                                    "매도 후 보유 수량", 
                                    "${holdingQuantity.toInt() - (sellQuantity.toIntOrNull() ?: 0)}주"
                                )
                            }
                        }
                        
                        Spacer(modifier = Modifier.height(24.dp))
                        
                        // 취소/확인 버튼
                        Row(
                            modifier = Modifier.fillMaxWidth(),
                            horizontalArrangement = Arrangement.spacedBy(8.dp)
                        ) {
                            Button(
                                onClick = { 
                                    hideKeyboard()
                                    showSellBottomSheet = false
                                    sellStep = 1
                                    sellOrderType = "지정가"
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
                            
                            Button(
                                onClick = { 
                                    hideKeyboard()
                                    isLoading = true
                                    // 1.5초 후에 다음 화면으로 이동
                                    MainScope().launch {
                                        delay(1500)
                                        isLoading = false
                                        sellStep = 3
                                        showSellSuccessMessage = true
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
                                        fontFamily = SCDreamFontFamily
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
                                    sellOrderType = "지정가"
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
                                    fontFamily = SCDreamFontFamily
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

        ModalBottomSheet(
            onDismissRequest = { 
                hideKeyboard()
                showBuyBottomSheet = false
                buyStep = 1
                buyOrderType = "지정가"
                buyPrice = ""
                buyQuantity = ""
                showSuccessMessage = false
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

                            // 현재 잔고
                            Text(
                                text = "현재 잔고",
                                fontSize = 14.sp,
                                color = Gray500,
                                fontFamily = SCDreamFontFamily
                            )
                            Text(
                                text = "${String.format("%,d", currentBalance)}원",
                                fontSize = 16.sp,
                                fontWeight = FontWeight.Bold,
                                color = Gray700,
                                fontFamily = SCDreamFontFamily
                            )
                            
                            Spacer(modifier = Modifier.height(24.dp))
                            
                            // 주문 유형 선택
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
                            
                            // 가격 입력
                            OutlinedTextField(
                                value = buyPrice,
                                onValueChange = { if (buyOrderType == "지정가") buyPrice = it },
                                label = { Text("주문 가격", fontFamily = SCDreamFontFamily) },
                                enabled = buyOrderType == "지정가",
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
                                    unfocusedBorderColor = Gray300
                                )
                            )
                            
                            Spacer(modifier = Modifier.height(8.dp))
                            
                            // 수량 입력
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
                                    .fillMaxWidth()
                                    .focusRequester(quantityFocusRequester),
                                colors = OutlinedTextFieldDefaults.colors(
                                    focusedBorderColor = Secondary500,
                                    unfocusedBorderColor = Gray300
                                )
                            )
                            
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
                                    fontFamily = SCDreamFontFamily
                                )
                            }
                        }
                    }
                    2 -> {
                        // 주문 확인 화면
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
                                OrderInfoRow("종목명", stock.stockName)
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
                            Button(
                                onClick = { 
                                    hideKeyboard()
                                    showBuyBottomSheet = false
                                    buyStep = 1
                                    buyOrderType = "지정가"
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
                            
                            Button(
                                onClick = { 
                                    hideKeyboard()
                                    isLoading = true
                                    // 1.5초 후에 다음 화면으로 이동
                                    MainScope().launch {
                                        delay(1500)
                                        isLoading = false
                                        buyStep = 3
                                        showSuccessMessage = true
                                    }
                                },
                                colors = ButtonDefaults.buttonColors(
                                    containerColor = Secondary500
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
                                        fontFamily = SCDreamFontFamily
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
                                    buyOrderType = "지정가"
                                    buyPrice = ""
                                    buyQuantity = ""
                                    showSuccessMessage = false
                                },
                                colors = ButtonDefaults.buttonColors(
                                    containerColor = Secondary500
                                ),
                                modifier = Modifier.fillMaxWidth()
                            ) {
                                Text(
                                    text = "확인",
                                    fontSize = 16.sp,
                                    fontFamily = SCDreamFontFamily
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
                    .height(56.dp),
                horizontalArrangement = Arrangement.SpaceBetween
            ) {
                // 매도 버튼 (왼쪽)
                Button(
                    onClick = { showSellBottomSheet = true },
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
                        fontSize = 18.sp,
                        fontWeight = FontWeight.W700,
                        fontFamily = SCDreamFontFamily,
                        color = Color.White
                    )
                }

                Spacer(modifier = Modifier.width(12.dp))

                // 매수 버튼 (오른쪽)
                Button(
                    onClick = { showBuyBottomSheet = true },
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
                        fontSize = 18.sp,
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
                        text = stock.stockName,
                        fontSize = 16.sp,
                        fontWeight = FontWeight.W700,
                        fontFamily = SCDreamFontFamily
                    )
                    Row(
                        verticalAlignment = Alignment.CenterVertically,
                    ) {
                        Text(
                            text = "${stock.currentPrice}원",
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
                        Text(
                            text = "${if (stock.fluctuationRate.startsWith("+")) "+" else "-"}${Math.abs(stock.currentPrice.toInt() * stock.fluctuationRate.replace("+", "").replace("%", "").toFloat() / 100)}원",
                            fontSize = 12.sp,
                            fontWeight = FontWeight.W500,
                            color = if (stock.fluctuationRate.startsWith("+")) Color.Red else Color.Blue,
                            fontFamily = SCDreamFontFamily
                        )
                        Text(
                            text = " (${stock.fluctuationRate}%)",
                            fontSize = 12.sp,
                            fontWeight = FontWeight.W500,
                            color = if (stock.fluctuationRate.startsWith("+")) Color.Red else Color.Blue,
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
                            .height(300.dp)
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
                                horizontalArrangement = Arrangement.SpaceBetween,
                                verticalAlignment = Alignment.CenterVertically
                            ) {
                                // 차트 전환 버튼
                                Button(
                                    onClick = { isShowingCandleChart = !isShowingCandleChart },
                                    modifier = Modifier
                                        .padding(vertical = 2.dp),
                                    colors = ButtonDefaults.buttonColors(
                                        containerColor = Gray200,
                                        contentColor = Gray700
                                    ),
                                    shape = RoundedCornerShape(8.dp)
                                ) {
                                    Text(
                                        text = if (isShowingCandleChart) "기본 차트 보기" else "캔들 차트 보기",
                                        fontSize = 12.sp,
                                        fontWeight = FontWeight.W500,
                                        fontFamily = SCDreamFontFamily
                                    )
                                }
                                
                                Row(horizontalArrangement = Arrangement.End) {
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
                            }

                            // 차트 표시
                            if (isShowingCandleChart) {
                                // 캔들 차트
                                CandleChartView(
                                    modifier = Modifier
                                        .fillMaxWidth()
                                        .height(300.dp)
                                        .padding(vertical = 8.dp)
                                )
                            } else {
                                // 기존 라인 차트
                                AndroidView(
                                    factory = { context ->
                                        LineChart(context).apply {
                                            description.isEnabled = false
                                            legend.isEnabled = false
                                            setTouchEnabled(true)
                                            setScaleEnabled(true)
                                            isDoubleTapToZoomEnabled = true

                                            xAxis.isEnabled = false
                                            axisLeft.isEnabled = false
                                            axisRight.isEnabled = false

                                            val entries = listOf(
                                                Entry(0f, 55000f),
                                                Entry(1f, 57000f),
                                                Entry(2f, 54000f),
                                                Entry(3f, 58000f),
                                                Entry(4f, 56000f),
                                                Entry(5f, 60000f),
                                                Entry(6f, 57000f),
                                                Entry(7f, 53000f)
                                            )

                                            val dataSet = LineDataSet(entries, "주가").apply {
                                                color = ChartRed.toArgb()
                                                setDrawCircles(false)
                                                setDrawValues(false)
                                                lineWidth = 2f
                                                mode = LineDataSet.Mode.LINEAR
                                                setDrawHorizontalHighlightIndicator(false)
                                                setDrawVerticalHighlightIndicator(true)
                                                highlightLineWidth = 1f
                                                highLightColor = Gray500.toArgb()
                                            }

                                            val dates = listOf(
                                                "2024-03-01", "2024-03-02", "2024-03-03", "2024-03-04",
                                                "2024-03-05", "2024-03-06", "2024-03-07", "2024-03-08"
                                            )

                                            val marker = ChartMarkerView(context, dates).apply {
                                                setBackgroundColor(android.graphics.Color.WHITE)
                                                setPadding(8, 8, 8, 8)
                                                setElevation(4f)
                                                setBackgroundResource(R.drawable.marker_background)
                                            }
                                            marker.chartView = this
                                            this.marker = marker

                                            // 최고/최저 Entry
                                            val maxEntry = entries.maxByOrNull { it.y }
                                            val minEntry = entries.minByOrNull { it.y }

                                            // 화면 경계를 고려한 레이블 위치 조정 로직
                                            val maxEntryX = maxEntry?.let {
                                                if (it.x >= entries.size - 2) {
                                                    it.x - 0.5f
                                                } else if (it.x <= 1) {
                                                    it.x + 0.5f
                                                } else it.x
                                            }
                                            
                                            val minEntryX = minEntry?.let {
                                                if (it.x >= entries.size - 2) {
                                                    it.x - 0.5f
                                                } else if (it.x <= 1) {
                                                    it.x + 0.5f
                                                } else it.x
                                            }

                                            val maxLabelEntry = maxEntry?.let { Entry(maxEntryX ?: it.x, it.y + 250f) }
                                            val minLabelEntry = minEntry?.let { Entry(minEntryX ?: it.x, it.y - 1150f) }

                                            val maxValueSet = LineDataSet(listOfNotNull(maxLabelEntry), "Max").apply {
                                                setDrawValues(true)
                                                setDrawCircles(false)
                                                color = AndroidColor.TRANSPARENT
                                                valueTextColor = Gray700.toArgb()
                                                valueTextSize = 12f
                                                setValueFormatter(object : ValueFormatter() {
                                                    override fun getPointLabel(entry: Entry?): String {
                                                        return "최고 ${entry?.y?.minus(250f)?.toInt()?.toString() ?: ""}"
                                                    }
                                                })
                                            }

                                            val minValueSet = LineDataSet(listOfNotNull(minLabelEntry), "Min").apply {
                                                setDrawValues(true)
                                                setDrawCircles(false)
                                                color = AndroidColor.TRANSPARENT
                                                valueTextColor = Gray700.toArgb()
                                                valueTextSize = 12f
                                                setValueFormatter(object : ValueFormatter() {
                                                    override fun getPointLabel(entry: Entry?): String {
                                                        return "최저 ${entry?.y?.plus(750f)?.toInt()?.toString() ?: ""}"
                                                    }
                                                })
                                            }

                                            var isHighlighting = false

                                            val initialDataSets = listOf<ILineDataSet>(dataSet, maxValueSet, minValueSet)
                                            data = LineData(initialDataSets)
                                            notifyDataSetChanged()

                                            fun updateChart() {
                                                maxValueSet.setDrawValues(!isHighlighting)
                                                minValueSet.setDrawValues(!isHighlighting)

                                                val dataSets = listOf<ILineDataSet>(
                                                    dataSet,
                                                    maxValueSet,
                                                    minValueSet
                                                )

                                                data = LineData(dataSets)
                                                invalidate()
                                            }

                                            setOnChartValueSelectedListener(object : OnChartValueSelectedListener {
                                                override fun onValueSelected(e: Entry?, h: Highlight?) {
                                                    isHighlighting = true
                                                    updateChart()
                                                }

                                                override fun onNothingSelected() {
                                                    isHighlighting = false
                                                    updateChart()
                                                }
                                            })

                                            setOnTouchListener { _, event ->
                                                when (event.action) {
                                                    android.view.MotionEvent.ACTION_DOWN -> {
                                                        parent?.requestDisallowInterceptTouchEvent(true)
                                                    }
                                                    android.view.MotionEvent.ACTION_UP, android.view.MotionEvent.ACTION_CANCEL -> {
                                                        parent?.requestDisallowInterceptTouchEvent(false)
                                                        highlightValue(null)
                                                        isHighlighting = false
                                                        updateChart()
                                                    }
                                                }
                                                false
                                            }

                                            updateChart()
                                        }
                                    },
                                    modifier = Modifier
                                        .fillMaxWidth()
                                        .height(300.dp)
                                        .padding(vertical = 8.dp)
                                )
                            }
                        }
                    }
                }

                item {
                    // 보유 정보
                    Card(
                        modifier = Modifier
                            .fillMaxWidth()
                            .padding(vertical = 16.dp),
                        colors = CardDefaults.cardColors(containerColor = Gray50)
                    ) {
                        Column(
                            modifier = Modifier
                                .fillMaxWidth()
                                .padding(16.dp)
                        ) {
                            // 보유 수량
                            Row(
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .padding(vertical = 4.dp),
                                horizontalArrangement = Arrangement.SpaceBetween
                            ) {
                                Text(
                                    text = "수량",
                                    fontSize = 14.sp,
                                    color = Gray500,
                                    fontFamily = SCDreamFontFamily
                                )
                                Text(
                                    text = "10.0589",
                                    fontSize = 14.sp,
                                    fontWeight = FontWeight.W500,
                                    color = Gray700,
                                    fontFamily = SCDreamFontFamily
                                )
                            }

                            // 평가금액
                            Row(
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .padding(vertical = 4.dp),
                                horizontalArrangement = Arrangement.SpaceBetween
                            ) {
                                Text(
                                    text = "평가금액",
                                    fontSize = 14.sp,
                                    color = Gray500,
                                    fontFamily = SCDreamFontFamily
                                )
                                Text(
                                    text = "$2,285.93",
                                    fontSize = 14.sp,
                                    fontWeight = FontWeight.W500,
                                    color = Gray700,
                                    fontFamily = SCDreamFontFamily
                                )
                            }

                            // 평단가
                            Row(
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .padding(vertical = 4.dp),
                                horizontalArrangement = Arrangement.SpaceBetween
                            ) {
                                Text(
                                    text = "평단가",
                                    fontSize = 14.sp,
                                    color = Gray500,
                                    fontFamily = SCDreamFontFamily
                                )
                                Text(
                                    text = "$227.30",
                                    fontSize = 14.sp,
                                    fontWeight = FontWeight.W500,
                                    color = Gray700,
                                    fontFamily = SCDreamFontFamily
                                )
                            }

                            // 총 수익
                            Row(
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .padding(vertical = 4.dp),
                                horizontalArrangement = Arrangement.SpaceBetween
                            ) {
                                Text(
                                    text = "총 수익",
                                    fontSize = 14.sp,
                                    color = Gray500,
                                    fontFamily = SCDreamFontFamily
                                )
                                Text(
                                    text = "$0.00 (0.00%)",
                                    fontSize = 14.sp,
                                    fontWeight = FontWeight.W500,
                                    color = Gray700,
                                    fontFamily = SCDreamFontFamily
                                )
                            }
                        }
                    }

                    // 호가창 (별도의 Card)
                    Card(
                        modifier = Modifier
                            .fillMaxWidth()
                            .padding(bottom = 16.dp),
                        colors = CardDefaults.cardColors(containerColor = Gray50)
                    ) {
                        Column(
                            modifier = Modifier
                                .fillMaxWidth()
                                .padding(16.dp)
                        ) {
                            Text(
                                text = "호가잔량",
                                fontSize = 16.sp,
                                fontWeight = FontWeight.Bold,
                                color = Gray700,
                                fontFamily = SCDreamFontFamily,
                                modifier = Modifier.padding(bottom = 12.dp)
                            )

                            // 매도 호가 (위쪽 5개)
                            val sellOrders = listOf(
                                Triple("1200", "152500", "650"),
                                Triple("900", "152000", "750"),
                                Triple("800", "151500", "850"),
                                Triple("700", "151000", "950"),
                                Triple("600", "150500", "1050")
                            )

                            val maxSellVolume = sellOrders.maxOf { it.first.toInt() }
                            val maxBuyVolume = sellOrders.maxOf { it.third.toInt() }

                            sellOrders.forEach { (askVolume, price, bidVolume) ->
                                Row(
                                    modifier = Modifier
                                        .fillMaxWidth()
                                        .padding(vertical = 2.dp),
                                    horizontalArrangement = Arrangement.SpaceBetween,
                                    verticalAlignment = Alignment.CenterVertically
                                ) {
                                    // 매도 잔량
                                    Box(
                                        modifier = Modifier
                                            .weight(1f)
                                            .height(24.dp)
                                    ) {
                                        Text(
                                            text = askVolume,
                                            fontSize = 12.sp,
                                            color = ChartBlue,
                                            fontFamily = SCDreamFontFamily,
                                            modifier = Modifier.align(Alignment.CenterEnd)
                                        )
                                        Box(
                                            modifier = Modifier
                                                .fillMaxWidth(askVolume.toFloat() / maxSellVolume)
                                                .height(24.dp)
                                                .background(
                                                    color = ChartBlue.copy(alpha = 0.1f),
                                                    shape = RoundedCornerShape(4.dp)
                                                )
                                                .align(Alignment.CenterEnd)
                                        )
                                    }

                                    // 가격
                                    Text(
                                        text = price,
                                        fontSize = 12.sp,
                                        color = Gray700,
                                        fontFamily = SCDreamFontFamily,
                                        modifier = Modifier
                                            .padding(horizontal = 12.dp)
                                            .width(60.dp),
                                        textAlign = TextAlign.Center
                                    )

                                    // 매수 잔량
                                    Box(
                                        modifier = Modifier
                                            .weight(1f)
                                            .height(24.dp)
                                    ) {
                                        Text(
                                            text = bidVolume,
                                            fontSize = 12.sp,
                                            color = ChartRed,
                                            fontFamily = SCDreamFontFamily,
                                            modifier = Modifier.align(Alignment.CenterStart)
                                        )
                                        Box(
                                            modifier = Modifier
                                                .fillMaxWidth(bidVolume.toFloat() / maxBuyVolume)
                                                .height(24.dp)
                                                .background(
                                                    color = ChartRed.copy(alpha = 0.1f),
                                                    shape = RoundedCornerShape(4.dp)
                                                )
                                                .align(Alignment.CenterStart)
                                        )
                                    }
                                }
                            }

                            // 현재가 표시
                            Row(
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .padding(vertical = 8.dp),
                                horizontalArrangement = Arrangement.Center
                            ) {
                                Text(
                                    text = "150000",
                                    fontSize = 14.sp,
                                    fontWeight = FontWeight.Bold,
                                    color = ChartRed,
                                    fontFamily = SCDreamFontFamily
                                )
                            }

                            // 매수 호가 (아래쪽 5개)
                            val buyOrders = listOf(
                                Triple("1200", "149500", "650"),
                                Triple("900", "149000", "750"),
                                Triple("800", "148500", "850"),
                                Triple("700", "148000", "950"),
                                Triple("600", "147500", "1050")
                            )

                            val maxSellVolume2 = buyOrders.maxOf { it.first.toInt() }
                            val maxBuyVolume2 = buyOrders.maxOf { it.third.toInt() }

                            buyOrders.forEach { (askVolume, price, bidVolume) ->
                                Row(
                                    modifier = Modifier
                                        .fillMaxWidth()
                                        .padding(vertical = 2.dp),
                                    horizontalArrangement = Arrangement.SpaceBetween,
                                    verticalAlignment = Alignment.CenterVertically
                                ) {
                                    // 매도 잔량
                                    Box(
                                        modifier = Modifier
                                            .weight(1f)
                                            .height(24.dp)
                                    ) {
                                        Text(
                                            text = askVolume,
                                            fontSize = 12.sp,
                                            color = ChartBlue,
                                            fontFamily = SCDreamFontFamily,
                                            modifier = Modifier.align(Alignment.CenterEnd)
                                        )
                                        Box(
                                            modifier = Modifier
                                                .fillMaxWidth(askVolume.toFloat() / maxSellVolume2)
                                                .height(24.dp)
                                                .background(
                                                    color = ChartBlue.copy(alpha = 0.1f),
                                                    shape = RoundedCornerShape(4.dp)
                                                )
                                                .align(Alignment.CenterEnd)
                                        )
                                    }

                                    // 가격
                                    Text(
                                        text = price,
                                        fontSize = 12.sp,
                                        color = Gray700,
                                        fontFamily = SCDreamFontFamily,
                                        modifier = Modifier
                                            .padding(horizontal = 12.dp)
                                            .width(60.dp),
                                        textAlign = TextAlign.Center
                                    )

                                    // 매수 잔량
                                    Box(
                                        modifier = Modifier
                                            .weight(1f)
                                            .height(24.dp)
                                    ) {
                                        Text(
                                            text = bidVolume,
                                            fontSize = 12.sp,
                                            color = ChartRed,
                                            fontFamily = SCDreamFontFamily,
                                            modifier = Modifier.align(Alignment.CenterStart)
                                        )
                                        Box(
                                            modifier = Modifier
                                                .fillMaxWidth(bidVolume.toFloat() / maxBuyVolume2)
                                                .height(24.dp)
                                                .background(
                                                    color = ChartRed.copy(alpha = 0.1f),
                                                    shape = RoundedCornerShape(4.dp)
                                                )
                                                .align(Alignment.CenterStart)
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
}

@Composable
private fun InfoRow(label: String, value: String) {
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .padding(vertical = 4.dp),
        horizontalArrangement = Arrangement.SpaceBetween
    ) {
        Text(
            text = label,
            fontSize = 16.sp,
            fontWeight = FontWeight.W400,
            color = Gray500,
            fontFamily = SCDreamFontFamily
        )
        Text(
            text = value,
            fontSize = 16.sp,
            fontWeight = FontWeight.W500,
            color = Gray700,
            fontFamily = SCDreamFontFamily
        )
    }
}

// 캔들 차트 Composable 추가
@Composable
fun CandleChartView(modifier: Modifier = Modifier) {
    val candleEntries = remember {
        listOf(
            CandleEntry(0f, 57000f, 54000f, 55000f, 57000f), // x, high, low, open, close
            CandleEntry(1f, 58000f, 56000f, 57000f, 56000f),
            CandleEntry(2f, 56000f, 53000f, 56000f, 54000f),
            CandleEntry(3f, 59000f, 57000f, 54000f, 58000f),
            CandleEntry(4f, 58000f, 55000f, 58000f, 56000f),
            CandleEntry(5f, 62000f, 59000f, 56000f, 60000f),
            CandleEntry(6f, 61000f, 56000f, 60000f, 57000f),
            CandleEntry(7f, 57000f, 52000f, 57000f, 53000f)
        )
    }
    
    val dates = remember {
        listOf(
            "2024-03-01", "2024-03-02", "2024-03-03", "2024-03-04",
            "2024-03-05", "2024-03-06", "2024-03-07", "2024-03-08"
        )
    }

    AndroidView(
        factory = { context ->
            CandleStickChart(context).apply {
                setBackgroundColor(android.graphics.Color.WHITE)
                description.isEnabled = false
                legend.isEnabled = false
                setTouchEnabled(true)

                setOnTouchListener { _, event ->
                    when (event.action) {
                        android.view.MotionEvent.ACTION_DOWN -> {
                            parent?.requestDisallowInterceptTouchEvent(true)
                        }
                        android.view.MotionEvent.ACTION_UP, android.view.MotionEvent.ACTION_CANCEL -> {
                            parent?.requestDisallowInterceptTouchEvent(false)
                            highlightValue(null)
                        }
                    }
                    false
                }

                val xAxis = xAxis
                xAxis.position = XAxis.XAxisPosition.BOTTOM
                xAxis.setDrawGridLines(false)
                xAxis.labelCount = 4
                xAxis.valueFormatter = object : ValueFormatter() {
                    override fun getFormattedValue(value: Float): String {
                        val index = value.toInt()
                        return if (index >= 0 && index < dates.size) {
                            dates[index].substring(5) // "MM-DD" 형식으로 표시
                        } else {
                            ""
                        }
                    }
                }

                axisRight.isEnabled = false
                
                axisLeft.setDrawGridLines(true)
                axisLeft.valueFormatter = object : ValueFormatter() {
                    override fun getFormattedValue(value: Float): String {
                        return "${value.toInt()}"
                    }
                }
                
                // 마커뷰 설정
                val candleMarkerView = ChartMarkerView(context, dates).apply {
                    setBackgroundColor(android.graphics.Color.WHITE)
                    setPadding(8, 8, 8, 8)
                    setElevation(4f)
                    setBackgroundResource(R.drawable.marker_background)
                }
                candleMarkerView.chartView = this
                marker = candleMarkerView

                val dataSet = CandleDataSet(candleEntries, "주가").apply {
                    color = android.graphics.Color.BLACK
                    shadowColorSameAsCandle = true
                    increasingColor = ChartRed.toArgb() // 상승 캔들 색상
                    increasingPaintStyle = Paint.Style.FILL
                    decreasingColor = ChartBlue.toArgb() // 하락 캔들 색상
                    decreasingPaintStyle = Paint.Style.FILL
                    neutralColor = Gray500.toArgb()
                    setDrawValues(false) // 값 표시 비활성화
                    setHighlightLineWidth(1.5f)
                    setHighLightColor(Gray700.toArgb())
                }

                val data = CandleData(dataSet)
                setData(data)
                invalidate()
            }
        },
        modifier = modifier
    )
}

// 캔들 차트용 마커뷰 클래스
class ChartMarkerView(
    context: android.content.Context,
    private val dates: List<String>
) : com.github.mikephil.charting.components.MarkerView(context, R.layout.candle_marker_view) {
    
    private val tvDate: android.widget.TextView = findViewById(R.id.tvDate)
    private val tvOpen: android.widget.TextView = findViewById(R.id.tvOpen)
    private val tvHigh: android.widget.TextView = findViewById(R.id.tvHigh)
    private val tvLow: android.widget.TextView = findViewById(R.id.tvLow)
    private val tvClose: android.widget.TextView = findViewById(R.id.tvClose)
    
    override fun refreshContent(e: Entry?, highlight: Highlight?) {
        if (e is CandleEntry) {
            val index = e.x.toInt()
            val date = if (index >= 0 && index < dates.size) dates[index] else ""
            
            tvDate.text = date
            tvOpen.text = "시가: ${e.open.toInt().toLocaleString()}원"
            tvHigh.text = "고가: ${e.high.toInt().toLocaleString()}원"
            tvLow.text = "저가: ${e.low.toInt().toLocaleString()}원"
            tvClose.text = "종가: ${e.close.toInt().toLocaleString()}원"
        }
        super.refreshContent(e, highlight)
    }
    
    override fun getOffset(): MPPointF {
        return MPPointF(-(width / 2f), -height.toFloat())
    }
}

// 숫자에 천 단위 콤마 추가 확장 함수
fun Int.toLocaleString(): String {
    return String.format("%,d", this)
}

@Composable
private fun OrderInfoRow(label: String, value: String) {
    Row(
        modifier = Modifier
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
            fontWeight = FontWeight.W500,
            color = Gray700,
            fontFamily = SCDreamFontFamily
        )
    }
}
