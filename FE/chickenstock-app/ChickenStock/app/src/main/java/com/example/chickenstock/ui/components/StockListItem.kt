package com.example.chickenstock.ui.components

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
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
import coil.compose.AsyncImage
import com.example.chickenstock.ui.theme.SCDreamFontFamily
import androidx.navigation.NavController
import com.example.chickenstock.navigation.Screen
import com.example.chickenstock.ui.theme.Gray300
import com.example.chickenstock.viewmodel.AuthViewModel
import com.example.chickenstock.viewmodel.MainViewModel
import android.widget.Toast
import androidx.compose.ui.platform.LocalContext

data class StockItem(
    val stockCode: String,
    val stockName: String,
    val market: String,
    val currentPrice: String,
    val priceChange: String = "",
    val fluctuationRate: String,
    val tradeAmount: String
)

private fun String.formatWithCommas(): String {
    return try {
        // +, - 부호와 쉼표 제거 후 Int로 변환
        val cleanNumber = this.replace("""[+\-,]""".toRegex(), "")
        val number = cleanNumber.toIntOrNull() ?: return this
        String.format("%,d", number)
    } catch (e: Exception) {
        this
    }
}

// 관심 종목 여부 체크 함수 (공통)
fun isInWatchlist(watchlist: Set<String>, stockCode: String): Boolean {
    return watchlist.any { it.trim().equals(stockCode.trim(), ignoreCase = true) }
}

// 종목 코드에서 _AL 등 접미사를 제거하는 함수
fun getPureStockCode(stockCode: String): String {
    return stockCode.substringBefore("_")
}

@Composable
fun StockListItem(
    stock: StockItem,
    navController: NavController,
    authViewModel: AuthViewModel,
    viewModel: MainViewModel,
    showContractStrength: Boolean = false,
    showTradeVolume: Boolean = false
) {
    var showLoginDialog by remember { mutableStateOf(false) }
    val context = LocalContext.current
    
    // 관심 종목 여부 확인
    val isInWatchlist = isInWatchlist(viewModel.watchlist.value, getPureStockCode(stock.stockCode))
    
    val isUp = stock.fluctuationRate.startsWith("+")
    val fluctuationColor = when {
        stock.fluctuationRate.startsWith("+") -> Color.Red
        stock.fluctuationRate.startsWith("-") -> Color.Blue
        else -> Color.Gray
    }

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
                    "관심 종목 등록을 위해서는 로그인이 필요합니다.",
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

    Surface(
        modifier = Modifier
            .fillMaxWidth()
            .padding(vertical = 8.dp)
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
        color = Color(0xFFF8F8F8),
        shadowElevation = 0.dp
    ) {
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .padding(8.dp),
            horizontalArrangement = Arrangement.SpaceBetween,
            verticalAlignment = Alignment.CenterVertically
        ) {
            // 왼쪽 정렬: 하트, 로고, 이름&금액
            Row(
                verticalAlignment = Alignment.CenterVertically,
                modifier = Modifier.weight(1f)
            ) {
                // 하트 아이콘
                IconButton(
                    onClick = {
                        if (authViewModel.isLoggedIn.value) {
                            if (isInWatchlist) {
                                viewModel.removeFromWatchlist(
                                    stockCode = getPureStockCode(stock.stockCode),
                                    context = context,
                                    onSuccess = {
                                        // 관심 종목 목록 새로고침
                                        viewModel.loadWatchlist(context)
                                    },
                                    onError = { errorMsg ->
                                        // 실패 시 Toast 메시지 표시
                                        Toast.makeText(context, errorMsg, Toast.LENGTH_SHORT).show()
                                    }
                                )
                            } else {
                                viewModel.addToWatchlist(
                                    stockCode = getPureStockCode(stock.stockCode),
                                    context = context,
                                    onSuccess = {
                                        // 관심 종목 목록 새로고침
                                        viewModel.loadWatchlist(context)
                                    },
                                    onError = { errorMsg ->
                                        // 실패 시 Toast 메시지 표시
                                        Toast.makeText(context, errorMsg, Toast.LENGTH_SHORT).show()
                                    }
                                )
                            }
                        } else {
                            showLoginDialog = true
                        }
                    },
                    modifier = Modifier.size(22.dp)
                ) {
                    Icon(
                        imageVector = if (isInWatchlist) Icons.Filled.Favorite else Icons.Outlined.FavoriteBorder,
                        contentDescription = if (isInWatchlist) "관심 종목 삭제" else "관심 종목 추가",
                        tint = if (isInWatchlist) Color(0xFFFF4081) else Gray300,
                        modifier = Modifier.size(22.dp)
                    )
                }

                Spacer(modifier = Modifier.width(8.dp))

                // 종목 로고
                Box(
                    modifier = Modifier
                        .size(40.dp)
                        .clip(CircleShape)
                        .background(Color.White)
                ) {
                    val imageUrl = "https://thumb.tossinvest.com/image/resized/96x0/https%3A%2F%2Fstatic.toss.im%2Fpng-icons%2Fsecurities%2Ficn-sec-fill-${getPureStockCode(stock.stockCode).trim().uppercase()}.png"
                    AsyncImage(
                        model = imageUrl,
                        contentDescription = "주식 로고",
                        modifier = Modifier.fillMaxSize(),
                        error = painterResource(id = com.example.chickenstock.R.drawable.logo) // 기본 이미지 리소스
                    )
                }
                
                // 회사 이름과 가격
                Column(
                    modifier = Modifier.padding(start = 8.dp)
                ) {
                    Text(
                        text = stock.stockName,
                        fontSize = 11.sp,
                        fontWeight = FontWeight.W700,
                        fontFamily = SCDreamFontFamily
                    )
                    Row(verticalAlignment = Alignment.CenterVertically) {
                        Text(
                            text = "${stock.currentPrice.formatWithCommas()}원",
                            fontSize = 9.sp,
                            fontWeight = FontWeight.W500,
                            fontFamily = SCDreamFontFamily
                        )
                        Text(
                            text = " ${stock.fluctuationRate}%",
                            fontSize = 9.sp,
                            fontWeight = FontWeight.W500,
                            color = fluctuationColor,
                            fontFamily = SCDreamFontFamily
                        )
                    }
                }
            }
            
            // 오른쪽 정렬: 거래대금/체결강도/거래량과 화살표
            Row(
                verticalAlignment = Alignment.CenterVertically,
                horizontalArrangement = Arrangement.End
            ) {
                Column(
                    horizontalAlignment = Alignment.End
                ) {
                    val (label, value, unit) = when {
                        showContractStrength -> Triple("체결강도", stock.tradeAmount, "%")
                        showTradeVolume -> Triple("거래량", stock.tradeAmount.formatWithCommas(), "주")
                        else -> Triple("거래대금", stock.tradeAmount.formatWithCommas(), "")
                    }
                    Row(verticalAlignment = Alignment.CenterVertically) {
                        Text(
                            text = label,
                            fontSize = 9.sp,
                            fontWeight = FontWeight.W500,
                            color = Color.Gray,
                            fontFamily = SCDreamFontFamily
                        )
                        if (!showContractStrength && !showTradeVolume) {
                            Text(
                                text = " (백만원)",
                                fontSize = 9.sp,
                                fontWeight = FontWeight.W500,
                                color = Color.Gray,
                                fontFamily = SCDreamFontFamily
                            )
                        }
                    }
                    Text(
                        text = value + if (showContractStrength) unit else if (showTradeVolume) unit else "",
                        fontSize = 11.sp,
                        fontWeight = FontWeight.W500,
                        fontFamily = SCDreamFontFamily,
                        color = Color.Black
                    )
                }
                Icon(
                    imageVector = Icons.Filled.KeyboardArrowRight,
                    contentDescription = "더 보기",
                    tint = Color.Gray,
                    modifier = Modifier
                        .padding(start = 4.dp)
                        .size(20.dp)
                )
            }
        }
    }
} 