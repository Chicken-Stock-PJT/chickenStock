package com.example.chickenstock.ui.screens.home

import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
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
import com.example.chickenstock.ui.components.MoreButton
import com.example.chickenstock.ui.components.StockItem
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import com.example.chickenstock.viewmodel.AuthViewModel
import com.example.chickenstock.api.RetrofitClient
import com.example.chickenstock.api.MemberService
import com.example.chickenstock.api.SimpleProfileResponse
import com.example.chickenstock.api.PortfolioWebSocket
import com.example.chickenstock.model.PortfolioData
import com.example.chickenstock.model.Position
import com.example.chickenstock.model.StockUpdate
import com.example.chickenstock.model.TotalData
import androidx.compose.ui.platform.LocalContext

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
    val stockName: String,
    val stockLogo: Int,
    val currentPrice: Int,
    val fluctuationRate: Float
)

// FavoriteStockItem 컴포넌트 추가
@Composable
fun FavoriteStockItem(
    stock: FavoriteStock,
    onFavoriteClick: () -> Unit,
    onItemClick: () -> Unit
) {
    Surface(
        modifier = Modifier
            .fillMaxWidth()
            .padding(vertical = 8.dp)
            .clickable(onClick = onItemClick),
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
                    model = stock.stockLogo,
                    contentDescription = "주식 로고",
                    modifier = Modifier
                        .size(40.dp)
                        .clip(CircleShape)
                        .background(Color.White)
                )
                Spacer(modifier = Modifier.width(12.dp))
                Column {
                    Text(
                        text = stock.stockName,
                        fontSize = 14.sp,
                        fontWeight = FontWeight.W700,
                        fontFamily = SCDreamFontFamily
                    )
                    Row(
                        verticalAlignment = Alignment.CenterVertically
                    ) {
                        Text(
                            text = "${stock.currentPrice.toFormattedString()}원",
                            fontSize = 12.sp,
                            fontWeight = FontWeight.W500,
                            fontFamily = SCDreamFontFamily
                        )
                        Text(
                            text = " ${if (stock.fluctuationRate > 0) "+" else ""}${String.format("%.2f", stock.fluctuationRate)}%",
                            color = when {
                                stock.fluctuationRate > 0 -> Color.Red
                                stock.fluctuationRate < 0 -> Color.Blue
                                else -> Color.Gray
                            },
                            fontSize = 12.sp,
                            fontWeight = FontWeight.W500,
                            fontFamily = SCDreamFontFamily
                        )
                    }
                }
            }
            
            // 오른쪽: 하트 아이콘
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

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun HomeScreen(
    navController: NavHostController,
    viewModel: MainViewModel,
    authViewModel: AuthViewModel
) {
    val sortOptions = listOf("거래대금", "급상승", "급하락", "거래량")
    var selectedSortIndex by remember { mutableStateOf(0) }
    
    // API 상태 관리
    var userProfile by remember { mutableStateOf<SimpleProfileResponse?>(null) }
    var portfolioData by remember { mutableStateOf<PortfolioData?>(null) }
    var isLoading by remember { mutableStateOf(false) }
    var error by remember { mutableStateOf<String?>(null) }

    // 임시 관심 종목 데이터
    val dummyFavoriteStocks = remember {
        listOf(
            StockItem(
                stockCode = "005930",
                stockName = "삼성전자",
                market = "KOSPI",
                currentPrice = "73,200",
                fluctuationRate = "-0.40",
                tradeAmount = "950"
            ),
            StockItem(
                stockCode = "000660",
                stockName = "SK하이닉스",
                market = "KOSPI",
                currentPrice = "174,100",
                fluctuationRate = "-3.50",
                tradeAmount = "850"
            ),
            StockItem(
                stockCode = "042660",
                stockName = "한화오션",
                market = "KOSPI",
                currentPrice = "77,100",
                fluctuationRate = "-2.80",
                tradeAmount = "750"
            )
        )
    }
    
    // 웹소켓 연결
    val webSocket = remember { 
        authViewModel.getToken()?.let { token ->
            PortfolioWebSocket(token)
        }
    }
    
    // 웹소켓 업데이트 수신
    LaunchedEffect(webSocket) {
        webSocket?.stockUpdateFlow?.collect { update ->
            update?.let { stockUpdate ->
                // 포트폴리오 데이터 업데이트
                portfolioData = portfolioData?.copy(
                    totalAsset = stockUpdate.totalData.totalAsset,
                    totalProfitLoss = stockUpdate.totalData.totalProfitLoss,
                    totalReturnRate = stockUpdate.totalData.totalReturnRate,
                    positions = portfolioData?.positions?.map { position ->
                        if (position.stockCode == stockUpdate.stockCode) {
                            position.copy(
                                currentPrice = stockUpdate.currentPrice,
                                valuationAmount = stockUpdate.valuationAmount,
                                profitLoss = stockUpdate.profitLoss,
                                returnRate = stockUpdate.returnRate
                            )
                        } else {
                            position
                        }
                    } ?: emptyList()
                )
            }
        }
    }

    // 웹소켓 연결 관리
    LaunchedEffect(authViewModel.isLoggedIn.value) {
        if (authViewModel.isLoggedIn.value) {
            webSocket?.connect()
        } else {
            webSocket?.disconnect()
        }
    }

    // 화면이 사라질 때 웹소켓 연결 해제
    DisposableEffect(Unit) {
        onDispose {
            webSocket?.disconnect()
        }
    }

    // API 호출
    val context = LocalContext.current
    val memberService = remember { RetrofitClient.getInstance(context).create(MemberService::class.java) }
    
    // 프로필 정보와 포트폴리오 정보 로드
    LaunchedEffect(authViewModel.isLoggedIn.value) {
        if (authViewModel.isLoggedIn.value) {
            isLoading = true
            try {
                // 프로필 정보 로드
                val profileResponse = memberService.getSimpleProfile()
                if (profileResponse.isSuccessful) {
                    userProfile = profileResponse.body()
                }
                
                // 포트폴리오 정보 로드
                val portfolioResponse = memberService.getPortfolio()
                if (portfolioResponse.isSuccessful) {
                    portfolioData = portfolioResponse.body()
                }
            } catch (e: Exception) {
                error = e.message ?: "알 수 없는 오류가 발생했습니다."
            } finally {
                isLoading = false
            }
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
            LazyColumn(modifier = Modifier.fillMaxWidth()) {
                // 로그인 상태에 따른 배너/자산 정보 영역
                items(1) {
                    if (authViewModel.isLoggedIn.value) {
                        // 로그인 상태: 자산 정보 표시
                        Column(
                            modifier = Modifier
                                .fillMaxWidth()
                                .background(Color.White)
                                .padding(horizontal = 34.dp, vertical = 8.dp)
                        ) {
                            // 인사말
                            Text(
                                text = "안녕하세요,",
                                fontSize = 30.sp,
                                fontWeight = FontWeight.W500,
                                fontFamily = SCDreamFontFamily
                            )

                            Spacer(modifier = Modifier.height(4.dp))

                            if (isLoading) {
                                CircularProgressIndicator(
                                    modifier = Modifier.size(24.dp),
                                    color = Color(0xFFFFEB3B)
                                )
                            } else if (error != null) {
                                Text(
                                    text = error!!,
                                    color = Color.Red,
                                    fontSize = 14.sp,
                                    fontFamily = SCDreamFontFamily
                                )
                            } else {
                                userProfile?.let { profile ->
                                    // 닉네임
                                    Text(
                                        text = "${profile.nickname}님",
                                        fontSize = 28.sp,
                                        fontWeight = FontWeight.W700,
                                        fontFamily = SCDreamFontFamily
                                    )
                                    
                                    Spacer(modifier = Modifier.height(24.dp))
                                    
                                    // 총 자산
                                    AssetInfoRow("총 자산", "${profile.memberMoney}원")
                                    
                                    Spacer(modifier = Modifier.height(8.dp))
                                    
                                    // 수익률
                                    Row(
                                        modifier = Modifier.fillMaxWidth(),
                                        horizontalArrangement = Arrangement.SpaceBetween
                                    ) {
                                        Text(
                                            text = "수익률",
                                            fontSize = 16.sp,
                                            fontWeight = FontWeight.W500,
                                            fontFamily = SCDreamFontFamily
                                        )
                                        Text(
                                            text = "${profile.returnRate}%",
                                            fontSize = 16.sp,
                                            fontWeight = FontWeight.W700,
                                            color = when {
                                                profile.returnRate.toFloat() > 0 -> Color.Red
                                                profile.returnRate.toFloat() < 0 -> Color.Blue
                                                else -> Color.Black
                                            },
                                            fontFamily = SCDreamFontFamily
                                        )
                                    }
                                }
                            }
                            
                            Spacer(modifier = Modifier.height(32.dp))
                            
                            // 보유 주식
                            Text(
                                text = "보유 주식",
                                fontSize = 18.sp,
                                fontWeight = FontWeight.W700,
                                fontFamily = SCDreamFontFamily
                            )
                            
                            Spacer(modifier = Modifier.height(16.dp))
                            
                            // 보유 주식 목록
                            if (portfolioData?.positions.isNullOrEmpty()) {
                                Text(
                                    text = "보유 주식이 없습니다.",
                                    fontSize = 14.sp,
                                    fontWeight = FontWeight.W500,
                                    fontFamily = SCDreamFontFamily,
                                    color = Color.Gray,
                                    modifier = Modifier.padding(vertical = 16.dp)
                                )
                            } else {
                                portfolioData?.positions?.take(3)?.forEach { position ->
                                    Surface(
                                        modifier = Modifier
                                            .fillMaxWidth()
                                            .padding(vertical = 8.dp),
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
                                            // 왼쪽: 로고와 종목명, 수량
                                            Row(
                                                verticalAlignment = Alignment.CenterVertically,
                                                modifier = Modifier.weight(1f)
                                            ) {
                                                AsyncImage(
                                                    model = "https://thumb.tossinvest.com/image/resized/96x0/https%3A%2F%2Fstatic.toss.im%2Fpng-icons%2Fsecurities%2Ficn-sec-fill-${position.stockCode}.png",
                                                    contentDescription = "주식 로고",
                                                    modifier = Modifier
                                                        .size(40.dp)
                                                        .clip(CircleShape)
                                                        .background(Color.White)
                                                )
                                                Spacer(modifier = Modifier.width(12.dp))
                                                Column {
                                                    Text(
                                                        text = position.stockName,
                                                        fontSize = 14.sp,
                                                        fontWeight = FontWeight.W700,
                                                        fontFamily = SCDreamFontFamily
                                                    )
                                                    Text(
                                                        text = "${position.quantity}주",
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
                                                // 평가금액
                                                Text(
                                                    text = "${position.valuationAmount.toFormattedString()} 원",
                                                    fontSize = 14.sp,
                                                    fontWeight = FontWeight.W700,
                                                    fontFamily = SCDreamFontFamily
                                                )
                                                // 손익과 수익률
                                                Text(
                                                    text = buildString {
                                                        if (position.profitLoss >= 0) append("+") else append("-")
                                                        append("${position.profitLoss.toFormattedString()}원")
                                                        append("(${if (position.returnRate >= 0) "+" else ""}${String.format("%.2f", position.returnRate)}%)")
                                                    },
                                                    color = when {
                                                        position.returnRate > 0 -> Color.Red
                                                        position.returnRate < 0 -> Color.Blue
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
                            }
                            
                            Spacer(modifier = Modifier.height(16.dp))
                            
                            // 보유 주식이 3개 초과일 때만 더보기 버튼 표시
                            if ((portfolioData?.positions?.size ?: 0) > 3) {
                                MoreButton(
                                    onClick = {
                                        navController.navigate(Screen.MyPage.route) {
                                            launchSingleTop = true
                                            popUpTo(Screen.Home.route)
                                        }
                                    },
                                    text = "보유 주식 더보기",
                                    modifier = Modifier.padding(horizontal = 16.dp)
                                )
                            }
                            
                            Spacer(modifier = Modifier.height(32.dp))
                            
                            // 관심 종목
                            Text(
                                text = "관심 종목",
                                fontSize = 18.sp,
                                fontWeight = FontWeight.W700,
                                fontFamily = SCDreamFontFamily
                            )
                            
                            Spacer(modifier = Modifier.height(16.dp))
                            
                            // 관심 종목 목록
                            dummyFavoriteStocks.forEach { stock ->
                                FavoriteStockItem(
                                    stock = FavoriteStock(
                                        stockName = stock.stockName,
                                        stockLogo = R.drawable.logo,
                                        currentPrice = stock.currentPrice.replace(",", "").toInt(),
                                        fluctuationRate = stock.fluctuationRate.toFloat()
                                    ),
                                    onFavoriteClick = { /* 즐겨찾기 기능 */ },
                                    onItemClick = { /* 종목 상세 화면으로 이동 */ }
                                )
                            }

                            Spacer(modifier = Modifier.height(16.dp))

                            // 관심 종목 더보기 버튼
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
                    } else {
                        // 비로그인 상태: 기존 배너 표시
                        Column(
                            modifier = Modifier
                                .fillMaxWidth()
                                .background(Color(0xFFFFFBEB))
                                .padding(vertical = 42.dp, horizontal = 24.dp)
                        ) {
                            Row(
                                modifier = Modifier.fillMaxWidth(),
                                verticalAlignment = Alignment.CenterVertically
                            ) {
                                Column(modifier = Modifier.weight(1f)) {
                                    Text("투자의 시작", fontSize = 32.sp, fontWeight = FontWeight.W700, fontFamily = SCDreamFontFamily)
                                    Spacer(modifier = Modifier.height(12.dp))
                                    Text("치킨스톡과 함께", fontSize = 32.sp, fontWeight = FontWeight.W700, fontFamily = SCDreamFontFamily)
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
                                    colors = ButtonDefaults.buttonColors(containerColor = Color(0xFFFFEB3B)),
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

                // 실시간 종목 랭킹 영역 (로그인 상태와 무관하게 표시)
                items(1) {
                    Column(
                        modifier = Modifier
                            .fillMaxWidth()
                            .background(Color.White)
                            .padding(vertical = 24.dp, horizontal = 32.dp)
                    ) {
                        Text("실시간 종목 랭킹", fontSize = 20.sp, fontWeight = FontWeight.W700, fontFamily = SCDreamFontFamily)
                        Spacer(modifier = Modifier.height(16.dp))

                        SegmentedControl(
                            items = sortOptions,
                            selectedIndex = selectedSortIndex,
                            onSelectedIndexChange = { selectedSortIndex = it }
                        )

                        Spacer(modifier = Modifier.height(12.dp))

                        val items = listOf(
                            StockItem("086520", "에코프로", "KOSDAQ", "1230000", "+5.60", "1200"),
                            StockItem("005930", "삼성전자", "KOSPI", "73200", "-0.40", "950"),
                            StockItem("035720", "카카오", "KOSPI", "48500", "-1.20", "850")
                        )

                        items.forEach { stock ->
                            StockListItem(
                                stock = stock,
                                navController = navController,
                                authViewModel = authViewModel,
                                onFavoriteClick = { /* 즐겨찾기 기능 */ }
                            )
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
