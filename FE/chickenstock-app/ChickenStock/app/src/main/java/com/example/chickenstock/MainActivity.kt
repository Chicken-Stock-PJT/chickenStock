package com.example.chickenstock

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.enableEdgeToEdge
import androidx.core.view.WindowCompat
import androidx.compose.animation.*
import androidx.compose.animation.core.*
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.ArrowBack
import androidx.compose.material.icons.filled.Home
import androidx.compose.material.icons.filled.Person
import androidx.compose.material.icons.filled.Search
import androidx.compose.material.icons.filled.ShowChart
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.TransformOrigin
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.navigation.compose.*
import androidx.lifecycle.viewmodel.compose.viewModel
import com.example.chickenstock.R
import com.example.chickenstock.navigation.NavGraph
import com.example.chickenstock.navigation.Screen
import com.example.chickenstock.ui.theme.ChickenStockTheme
import com.example.chickenstock.ui.theme.Gray0
import com.example.chickenstock.ui.theme.Gray700
import com.example.chickenstock.ui.theme.SCDreamFontFamily
import com.example.chickenstock.viewmodel.MainViewModel
import com.example.chickenstock.viewmodel.AuthViewModel
import androidx.compose.ui.platform.LocalSoftwareKeyboardController
import androidx.compose.ui.platform.LocalContext
import androidx.compose.runtime.LaunchedEffect
import androidx.lifecycle.lifecycleScope
import com.example.chickenstock.api.RetrofitClient
import com.example.chickenstock.api.StockService
import com.example.chickenstock.data.StockRepository
import kotlinx.coroutines.launch
import com.example.chickenstock.api.AuthService
import android.util.Log
import android.content.pm.PackageManager
import android.content.pm.PackageInfo
import android.util.Base64
import java.security.MessageDigest
import com.example.chickenstock.data.TokenManager
import android.content.Intent
import androidx.compose.ui.focus.onFocusChanged
import androidx.compose.ui.focus.FocusState
import androidx.compose.ui.platform.LocalFocusManager

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        
        // 딥링크로 들어온 경우 처리
        handleDeepLink(intent?.data)
        
        // 토큰 값 로그 출력
        val tokenManager = TokenManager.getInstance(this)
        val accessToken = tokenManager.getAccessToken()
        val refreshToken = tokenManager.getRefreshToken()
        val prefs = tokenManager.getSharedPreferences()
        Log.d("TokenInfo", "Access Token: $accessToken")
        Log.d("TokenInfo", "Refresh Token: $refreshToken")
        Log.d("TokenInfo", "All SharedPreferences: ${prefs.all}")
        
        // 키 해시 얻기
        try {
            val packageInfo: PackageInfo = if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.P) {
                packageManager.getPackageInfo(packageName, PackageManager.GET_SIGNING_CERTIFICATES)
            } else {
                @Suppress("DEPRECATION")
                packageManager.getPackageInfo(packageName, PackageManager.GET_SIGNATURES)
            }

            val signatures = if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.P) {
                packageInfo.signingInfo?.apkContentsSigners
            } else {
                @Suppress("DEPRECATION")
                packageInfo.signatures
            }

            signatures?.forEach { signature ->
                val md = MessageDigest.getInstance("SHA")
                md.update(signature.toByteArray())
                val keyHash = Base64.encodeToString(md.digest(), Base64.DEFAULT)
                Log.d("KeyHash", "키 해시 값 : $keyHash")
            }
        } catch (e: Exception) {
            Log.e("KeyHash", "키 해시 추출 실패", e)
        }
        
        // 상태바 숨기기
        WindowCompat.setDecorFitsSystemWindows(window, false)
        
        enableEdgeToEdge()

        // 앱 실행 시 1회만 주식 데이터 가져오기
        if (!StockRepository.isInitialized()) {
            lifecycleScope.launch {
                try {
                    val stockService = RetrofitClient.getInstance(this@MainActivity).create(StockService::class.java)
                    val response = stockService.getAllStocks()
                    if (response.isSuccessful) {
                        response.body()?.let { stocks ->
                            StockRepository.setStocks(stocks)
                        }
                    }
                } catch (e: Exception) {
                    // 에러 처리
                }
            }
        }

        val authService = RetrofitClient.getInstance(this).create(AuthService::class.java)

        setContent {
            ChickenStockTheme {
                MainScreen()
            }
        }
    }

    private fun handleDeepLink(uri: android.net.Uri?) {
        uri?.let {
            when {
                it.scheme == "chickenstock" && it.host == "oauth2callback" -> {
                    // OAuth2 콜백 처리
                    val code = it.getQueryParameter("code")
                    val state = it.getQueryParameter("state")
                    val error = it.getQueryParameter("error")
                    
                    Log.d("OAuth2Callback", "Received callback - code: $code, state: $state, error: $error")
                    
                    if (error != null) {
                        // 에러 처리
                        Log.e("OAuth2Callback", "OAuth2 error: $error")
                        // TODO: 에러 메시지 표시
                        return
                    }
                    
                    if (code != null) {
                        // 인증 코드를 사용하여 토큰 요청
                        lifecycleScope.launch {
                            try {
                                val authService = RetrofitClient.getInstance(this@MainActivity)
                                    .create(AuthService::class.java)
                                    
                                // TODO: 토큰 요청 API 호출
                                // val response = authService.getToken(code)
                                
                                // TODO: 토큰 저장
                                // val tokenManager = TokenManager.getInstance(this@MainActivity)
                                // tokenManager.saveTokens(response.accessToken, response.refreshToken)
                                
                                Log.d("OAuth2Callback", "Token request successful")
                            } catch (e: Exception) {
                                Log.e("OAuth2Callback", "Token request failed", e)
                                // TODO: 에러 메시지 표시
                            }
                        }
                    }
                }
            }
        }
    }

    override fun onNewIntent(intent: Intent) {
        super.onNewIntent(intent)
        handleDeepLink(intent.data)
    }
}

@OptIn(ExperimentalAnimationApi::class)
@Composable
fun SearchTopAppBar(
    isSearchExpanded: Boolean,
    searchText: String,
    onSearchTextChange: (String) -> Unit,
    onBackClick: () -> Unit,
    onSearchIconClick: () -> Unit
) {
    val keyboardController = LocalSoftwareKeyboardController.current
    val focusManager = LocalFocusManager.current
    var isSearchFieldFocused by remember { mutableStateOf(false) }
    
    val iconOffsetX by animateDpAsState(
        targetValue = if (isSearchExpanded) (-45).dp else 0.dp,
        animationSpec = tween(300),
        label = "iconOffsetX"
    )

    val textFieldWidth by animateDpAsState(
        targetValue = if (isSearchExpanded) 300.dp else 0.dp,
        animationSpec = tween(300),
        label = "textFieldWidth"
    )

    val alpha by animateFloatAsState(
        targetValue = if (isSearchExpanded) 1f else 0f,
        animationSpec = tween(300),
        label = "alpha"
    )

    Surface(
        color = Gray0,
        modifier = Modifier
            .fillMaxWidth()
            .height(64.dp)
    ) {
        Row(
            modifier = Modifier
                .fillMaxSize()
                .padding(horizontal = 16.dp),
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.SpaceBetween
        ) {
            // 🔁 왼쪽: 로고 ↔ ← 아이콘
            AnimatedContent(
                targetState = isSearchExpanded,
                transitionSpec = {
                    fadeIn(tween(200)) with fadeOut(tween(100))
                },
                label = "logo-back-button"
            ) { expanded ->
                if (expanded) {
                    IconButton(onClick = { 
                        keyboardController?.hide()
                        focusManager.clearFocus()
                        onBackClick() 
                    }) {
                        Icon(
                            imageVector = Icons.Default.ArrowBack,
                            contentDescription = "뒤로가기",
                            tint = Gray700,
                            modifier = Modifier
                                .size(30.dp)
                        )
                    }
                } else {
                    Icon(
                        painter = painterResource(id = R.drawable.logo),
                        contentDescription = "로고",
                        tint = Color.Unspecified,
                        modifier = Modifier
                            .size(52.dp)
                            .padding(start = 4.dp)
                    )
                }
            }

            // 🔁 오른쪽: 검색 아이콘 + TextField 애니메이션
            Box(
                modifier = Modifier
                    .wrapContentSize()
            ) {
                // 검색 아이콘 (이동)
                IconButton(
                    onClick = {
                        onSearchIconClick()
                        isSearchFieldFocused = true
                    },
                    modifier = Modifier.offset(x = iconOffsetX)
                ) {
                    Icon(
                        imageVector = Icons.Default.Search,
                        contentDescription = "검색",
                        tint = Gray700,
                        modifier = Modifier.size(30.dp)
                    )
                }

                // 검색창 (아이콘 오른쪽에서 확장됨)
                Box(
                    modifier = Modifier
                        .offset(x = iconOffsetX + 40.dp, y = 3.dp)
                        .width(textFieldWidth)
                        .height(40.dp)
                        .graphicsLayer(alpha = alpha)
                        .background(Color(0xFFF5F5F5), shape = MaterialTheme.shapes.medium),
                    contentAlignment = Alignment.CenterStart
                ) {
                    BasicTextField(
                        value = searchText,
                        onValueChange = onSearchTextChange,
                        singleLine = true,
                        textStyle = TextStyle(
                            color = Gray700,
                            fontSize = 16.sp,
                            fontFamily = SCDreamFontFamily
                        ),
                        modifier = Modifier
                            .fillMaxWidth()
                            .padding(horizontal = 12.dp)
                            .onFocusChanged { focusState ->
                                isSearchFieldFocused = focusState.isFocused
                            },
                        decorationBox = { innerTextField ->
                            Box(modifier = Modifier.fillMaxWidth()) {
                                if (searchText.isEmpty()) {
                                    Text(
                                        text = "종목을 검색하세요",
                                        color = Gray700.copy(alpha = 0.5f),
                                        fontSize = 16.sp,
                                        fontFamily = SCDreamFontFamily
                                    )
                                }
                                innerTextField()
                            }
                        }
                    )
                }
            }
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun MainScreen(
    viewModel: MainViewModel = viewModel(),
    authViewModel: AuthViewModel = viewModel(factory = AuthViewModel.Factory(LocalContext.current))
) {
    val navController = rememberNavController()
    val currentBackStack by navController.currentBackStackEntryAsState()
    val currentRoute = currentBackStack?.destination?.route ?: Screen.Home.route
    var searchText by remember { mutableStateOf("") }
    var isSearchExpanded by remember { mutableStateOf(false) }
    
    LaunchedEffect(currentRoute) {
        isSearchExpanded = currentRoute.startsWith("search")
    }

    // 검색어가 변경될 때마다 SearchScreen으로 전달
    LaunchedEffect(searchText) {
        if (isSearchExpanded) {
            navController.currentBackStackEntry?.savedStateHandle?.set("searchQuery", searchText)
        }
    }

    // 검색 화면으로 이동할 때 검색어 초기화
    LaunchedEffect(isSearchExpanded) {
        if (isSearchExpanded) {
            navController.currentBackStackEntry?.savedStateHandle?.set("searchQuery", searchText)
        }
    }

    // 현재 route에 따라 selectedIndex 업데이트
    LaunchedEffect(currentRoute) {
        when {
            currentRoute.startsWith(Screen.Home.route) -> viewModel.updateSelectedIndex(0)
            currentRoute.startsWith(Screen.Stock.route) -> viewModel.updateSelectedIndex(1)
            currentRoute.startsWith(Screen.MyPage.route) -> viewModel.updateSelectedIndex(2)
        }
    }

    val tabList = listOf(Screen.Home.route, Screen.Stock.route, Screen.MyPage.route)
    val selectedIndex by viewModel.selectedIndex
    val isBottomBarVisible by viewModel.isBottomBarVisible

    // 현재 라우트가 변경될 때 bottomBar 가시성 업데이트
    LaunchedEffect(currentRoute) {
        viewModel.setBottomBarVisibility(!currentRoute.startsWith("search") && 
                                       !currentRoute.startsWith("login") && 
                                       !currentRoute.startsWith("signup") &&
                                       !currentRoute.startsWith("findpw"))
    }

    // 로그인 상태 변경 감지
    LaunchedEffect(authViewModel.isLoggedIn.value) {
        if (authViewModel.isLoggedIn.value) {
            // 로그인 시 홈으로 이동하고 하단 네비바도 홈으로 변경
            viewModel.updateSelectedIndex(0)
            navController.navigate(Screen.Home.route) {
                popUpTo(navController.graph.startDestinationId) {
                    inclusive = true
                }
            }
        }
    }

    // 하단 네비바 애니메이션
    val bottomBarOffset by animateFloatAsState(
        targetValue = if (isBottomBarVisible) 0f else 1f,
        animationSpec = spring(
            dampingRatio = Spring.DampingRatioMediumBouncy,
            stiffness = Spring.StiffnessLow
        ),
        label = "bottomBarOffset"
    )

    Scaffold(
        containerColor = Gray0,
        contentWindowInsets = WindowInsets(0, 0, 0, 0),
        modifier = Modifier.padding(WindowInsets.systemBars.asPaddingValues()),
        topBar = {
            if (!currentRoute.startsWith("login") && 
                !currentRoute.startsWith("signup") && 
                !currentRoute.startsWith("findpw")) {
                SearchTopAppBar(
                    isSearchExpanded = isSearchExpanded,
                    searchText = searchText,
                    onSearchTextChange = { 
                        searchText = it
                        // 검색어가 변경될 때마다 즉시 SearchScreen에 전달
                        navController.currentBackStackEntry?.savedStateHandle?.set("searchQuery", it)
                    },
                    onBackClick = {
                        isSearchExpanded = false
                        navController.navigateUp()
                    },
                    onSearchIconClick = {
                        isSearchExpanded = true
                        navController.navigate("search") {
                            launchSingleTop = true
                        }
                    }
                )
            }
        },
        bottomBar = {
            Box(
                modifier = Modifier
                    .graphicsLayer {
                        translationY = bottomBarOffset * 200f  // 200dp만큼 아래로 이동
                    }
            ) {
                AnimatedBottomBar(
                    selectedIndex = selectedIndex,
                    onTabSelected = { index ->
                        viewModel.updateSelectedIndex(index)
                        val target = tabList[index]
                        if (currentRoute.startsWith("stock_detail")) {
                            val previousRoute = navController.previousBackStackEntry?.destination?.route ?: Screen.Home.route
                            if (previousRoute != target) {
                                navController.navigate(target) {
                                    launchSingleTop = true
                                    popUpTo(navController.graph.startDestinationId) {
                                        saveState = true
                                    }
                                }
                            }
                        } else {
                            navController.navigate(target) {
                                launchSingleTop = true
                                popUpTo(navController.graph.startDestinationId) {
                                    saveState = true
                                }
                            }
                        }
                    }
                )
            }
        }
    ) { innerPadding ->
        NavGraph(
            navController = navController,
            modifier = Modifier.padding(innerPadding),
            viewModel = viewModel,
            authViewModel = authViewModel
        )
    }
}

@Composable
fun AnimatedBottomBar(
    selectedIndex: Int,
    onTabSelected: (Int) -> Unit
) {
    val indicatorWidth = 60.dp
    val indicatorHeight = 64.dp

    BoxWithConstraints(
        modifier = Modifier
            .fillMaxWidth()
            .height(72.dp)
            .background(Color.White)
    ) {
        val itemWidth = maxWidth / 3
        val centerOffset = (itemWidth - indicatorWidth) / 2
        
        val indicatorOffset by animateDpAsState(
            targetValue = itemWidth * selectedIndex + centerOffset,
            animationSpec = tween(durationMillis = 250),
            label = "indicatorOffset"
        )

        // 🟡 움직이는 배경 박스
        Box(
            modifier = Modifier
                .offset(x = indicatorOffset)
                .width(indicatorWidth)
                .height(indicatorHeight)
                .background(Color(0xFFFEE689), shape = MaterialTheme.shapes.small)
        )

        // 📦 탭 항목
        Row(modifier = Modifier.fillMaxSize()) {
            listOf(
                Pair("홈", Icons.Filled.Home),
                Pair("주식", Icons.Filled.ShowChart),
                Pair("내 기록", Icons.Filled.Person)
            ).forEachIndexed { index, (label, icon) ->
                val isSelected = index == selectedIndex
                val iconColor by animateColorAsState(
                    targetValue = if (isSelected) Color.Red else Color.Black,
                    animationSpec = tween(durationMillis = 200),
                    label = "iconColor"
                )
                Box(
                    modifier = Modifier
                        .width(itemWidth)
                        .fillMaxHeight(),
                    contentAlignment = Alignment.Center
                ) {
                    Box(
                        modifier = Modifier
                            .width(indicatorWidth)
                            .height(indicatorHeight)
                            .then(
                                if (!isSelected) {
                                    Modifier.clickable(
                                        indication = null,
                                        interactionSource = remember { MutableInteractionSource() }
                                    ) { onTabSelected(index) }
                                } else {
                                    Modifier
                                }
                            ),
                        contentAlignment = Alignment.Center
                    ) {
                        Column(horizontalAlignment = Alignment.CenterHorizontally) {
                            Icon(icon, contentDescription = label, tint = iconColor)
                            Text(
                                text = label,
                                color = iconColor,
                                fontSize = 12.sp,
                                fontFamily = SCDreamFontFamily
                            )
                        }
                    }
                }
            }
        }
    }
}