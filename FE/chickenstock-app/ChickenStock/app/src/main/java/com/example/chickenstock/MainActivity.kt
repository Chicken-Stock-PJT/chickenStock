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
import androidx.compose.material.icons.filled.Star
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
import androidx.navigation.NavHostController
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
import androidx.compose.ui.unit.LayoutDirection
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
import androidx.navigation.NavGraph.Companion.findStartDestination
import com.example.chickenstock.api.TokenRefreshRequest
import com.example.chickenstock.ui.screens.SplashScreen
import com.google.accompanist.systemuicontroller.rememberSystemUiController
import android.os.Build
import androidx.core.splashscreen.SplashScreen.Companion.installSplashScreen
import android.app.NotificationManager
import android.content.Context

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        // Android 12 이상에서 시스템 스플래시 바로 넘기기
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            installSplashScreen().setKeepOnScreenCondition { false }
        }
        super.onCreate(savedInstanceState)
        
        // AuthViewModel 생성 및 RetrofitClient에 설정
        val authViewModel = AuthViewModel.Factory(this).create(AuthViewModel::class.java)
        RetrofitClient.setAuthViewModel(authViewModel)
        
        // MainViewModel 생성
        val mainViewModel = MainViewModel()
        
        // 알림 권한 확인
        mainViewModel.checkAndRequestNotificationPermission(this)
        
        // 딥링크로 들어온 경우 처리
        handleDeepLink(intent?.data)
        
        // 소셜 로그인 등에서 바로 홈으로 이동해야 하는 경우 splashDone을 true로 설정
        val destination = intent?.getStringExtra("destination")
        if (destination == com.example.chickenstock.navigation.Screen.Home.route) {
            mainViewModel.setSplashDone()
        }
        
        // 토큰 값 로그 출력
        val tokenManager = TokenManager.getInstance(this)
        val accessToken = tokenManager.getAccessToken()
        val refreshToken = tokenManager.getRefreshToken()
        val expiresIn = tokenManager.getAccessTokenExpiresIn()
        val prefs = tokenManager.getSharedPreferences()
        Log.d("TokenInfo", "Access Token: $accessToken")
        Log.d("TokenInfo", "Refresh Token: $refreshToken")
        Log.d("TokenInfo", "All SharedPreferences: ${prefs.all}")
        
        // 토큰 상태 확인
        if (accessToken != null && refreshToken != null) {
            // 만료 시간 확인
            val currentTime = System.currentTimeMillis()
            val expiresAt = expiresIn // expiresIn은 절대 만료 시각(타임스탬프)
            val timeRemaining = expiresAt - currentTime
            
            Log.d("TokenInfo", "Token expires in: "+timeRemaining+"ms")
            
            // 토큰 갱신 비활성화
            // if (timeRemaining < 10 * 60 * 1000) {
            //     Log.d("TokenInfo", "토큰 만료가 임박하거나 이미 만료되었습니다. 갱신을 시도합니다.")
            //     lifecycleScope.launch {
            //         try {
            //             // Retrofit 인스턴스 초기화 (기존 인스턴스 문제 해결)
            //             RetrofitClient.resetInstance()
            //
            //             val authService = RetrofitClient.getInstance(this@MainActivity)
            //                 .create(AuthService::class.java)
            //             val response = authService.refreshAllTokens(
            //                 TokenRefreshRequest(refreshToken)
            //             )
            //
            //             if (response.isSuccessful && response.body() != null) {
            //                 val newTokens = response.body()!!
            //                 tokenManager.saveTokens(
            //                     newTokens.accessToken,
            //                     newTokens.refreshToken,
            //                     newTokens.accessTokenExpiresIn, // 절대 만료 시각 그대로 저장
            //                     30 * 24 * 60 * 60 * 1000L // 리프레시 토큰 30일
            //                 )
            //                 Log.d("TokenInfo", "토큰 갱신 성공")
            //             } else {
            //                 Log.e("TokenInfo", "토큰 갱신 실패: ${response.code()}")
            //                 // 토큰 갱신 실패 시, 기존 토큰 삭제 및 인스턴스 초기화
            //                 tokenManager.clearTokens()
            //                 RetrofitClient.resetInstance()
            //             }
            //         } catch (e: Exception) {
            //             Log.e("TokenInfo", "토큰 갱신 중 오류: ${e.message}", e)
            //             // 예외 발생 시 Retrofit 인스턴스 재설정
            //             RetrofitClient.resetInstance()
            //         }
            //     }
            // }
        } else {
            // 토큰이 없는 경우
            Log.d("TokenInfo", "토큰이 없습니다. 로그인이 필요합니다.")
        }
        
        // 키 해시 얻기
        getKeyHash()
        
        // 상태바 숨기기
        WindowCompat.setDecorFitsSystemWindows(window, false)
        
        enableEdgeToEdge()

        // 앱 실행 시 1회만 주식 데이터 가져오기
        if (!StockRepository.isInitialized()) {
            lifecycleScope.launch {
                try {
                    val stockService = RetrofitClient.getInstance(this@MainActivity, ignoreAuthCheck = true).create(StockService::class.java)
                    val response = stockService.getAllStocks()
                    if (response.isSuccessful) {
                        response.body()?.let { stocks ->
                            StockRepository.setStocks(stocks)
                        }
                    }
                } catch (e: Exception) {
                    Log.e("MainActivity", "주식 데이터 로드 실패", e)
                }
            }
        }

        val authService = RetrofitClient.getInstance(this).create(AuthService::class.java)

        // Retrofit 인스턴스 초기화
        val retrofit = RetrofitClient.getInstance(this)
        Log.d("RetrofitInit", "Retrofit 인스턴스 초기화 완료")

        setContent {
            ChickenStockTheme {
                MainScreen(viewModel = mainViewModel, authViewModel = authViewModel)
            }
        }
    }

    private fun getKeyHash() {
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
                        return
                    }
                    
                    if (code != null) {
                        // 인증 코드를 사용하여 토큰 요청
                        lifecycleScope.launch {
                            try {
                                val authService = RetrofitClient.getInstance(this@MainActivity)
                                    .create(AuthService::class.java)
                                
                                Log.d("OAuth2Callback", "Token request successful")
                            } catch (e: Exception) {
                                Log.e("OAuth2Callback", "Token request failed", e)
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
        color = Color(0xFFF5F5F5),
        modifier = Modifier
            .fillMaxWidth()
            .height(56.dp)
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
                                .size(26.dp)
                        )
                    }
                } else {
                    Icon(
                        painter = painterResource(id = R.drawable.logo),
                        contentDescription = "로고",
                        tint = Color.Unspecified,
                        modifier = Modifier
                            .size(40.dp)
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
                    modifier = Modifier
                        .offset(x = iconOffsetX)
                        .size(42.dp)
                ) {
                    Icon(
                        imageVector = Icons.Default.Search,
                        contentDescription = "검색",
                        tint = Gray700,
                        modifier = Modifier.size(26.dp)
                    )
                }

                // 검색창 (아이콘 오른쪽에서 확장됨)
                Box(
                    modifier = Modifier
                        .offset(x = iconOffsetX + 40.dp, y = 3.dp)
                        .width(textFieldWidth)
                        .height(36.dp)
                        .graphicsLayer(alpha = alpha)
                        .background(Color.White, shape = MaterialTheme.shapes.medium),
                    contentAlignment = Alignment.CenterStart
                ) {
                    BasicTextField(
                        value = searchText,
                        onValueChange = onSearchTextChange,
                        singleLine = true,
                        textStyle = TextStyle(
                            color = Gray700,
                            fontSize = 14.sp,
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
                                        fontSize = 14.sp,
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
    viewModel: MainViewModel,
    authViewModel: AuthViewModel
) {
    val navController = rememberNavController()
    val navBackStackEntry by navController.currentBackStackEntryAsState()
    val currentRoute = navBackStackEntry?.destination?.route ?: ""
    val context = LocalContext.current
    
    // 알림 권한 다이얼로그
    if (viewModel.showNotificationPermissionDialog.value) {
        AlertDialog(
            onDismissRequest = { viewModel.setShowNotificationPermissionDialog(false) },
            title = { Text("알림 권한") },
            text = { Text("주식 거래 알림을 받으시려면 알림 권한이 필요합니다. 알림을 허용하시겠습니까?") },
            confirmButton = {
                TextButton(
                    onClick = {
                        viewModel.setShowNotificationPermissionDialog(false)
                        val intent = Intent().apply {
                            action = android.provider.Settings.ACTION_APP_NOTIFICATION_SETTINGS
                            putExtra(android.provider.Settings.EXTRA_APP_PACKAGE, context.packageName)
                        }
                        context.startActivity(intent)
                    }
                ) {
                    Text("허용")
                }
            },
            dismissButton = {
                TextButton(
                    onClick = { viewModel.setShowNotificationPermissionDialog(false) }
                ) {
                    Text("거부")
                }
            }
        )
    }
    
    Log.d("RouteCheck", "currentRoute: $currentRoute")
    val isSplash = currentRoute?.lowercase()?.startsWith("splash") == true
    var searchText by remember { mutableStateOf("") }
    var isSearchExpanded by remember { mutableStateOf(false) }
    
    val systemUiController = rememberSystemUiController()
    val useDarkIcons = true
    SideEffect {
        systemUiController.setStatusBarColor(
            color = Color(0xFFF5F5F5),
            darkIcons = useDarkIcons
        )
    }

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
        if (currentRoute.isNotEmpty()) {
            when {
                currentRoute.startsWith(Screen.Home.route) -> viewModel.updateSelectedIndex(0)
                currentRoute.startsWith(Screen.Stock.route) || currentRoute.startsWith("stock_detail") -> viewModel.updateSelectedIndex(1)
                currentRoute.startsWith(Screen.Ranking.route) -> viewModel.updateSelectedIndex(2)
                currentRoute.startsWith(Screen.MyPage.route) -> viewModel.updateSelectedIndex(3)
            }
        }
    }

    val tabList = listOf(Screen.Home.route, Screen.Stock.route, Screen.Ranking.route, Screen.MyPage.route)
    val selectedIndex by viewModel.selectedIndex
    val isBottomBarVisible by viewModel.isBottomBarVisible

    // 현재 라우트가 변경될 때 bottomBar 가시성 업데이트
    LaunchedEffect(currentRoute) {
        viewModel.setBottomBarVisibility(!isSplash &&
            !currentRoute.startsWith("search") && 
            !currentRoute.startsWith("login") && 
            !currentRoute.startsWith("signup") &&
            !currentRoute.startsWith("findpw") &&
            !currentRoute.startsWith("terms_agreement") &&
            !currentRoute.startsWith("chat"))
    }

    // 로그인 상태 변경 감지
    LaunchedEffect(authViewModel.isLoggedIn.value) {
        if (currentRoute.isNotEmpty() && authViewModel.isLoggedIn.value) {
            // 로그인 시 홈으로 이동하고 하단 네비바도 홈으로 변경
            viewModel.updateSelectedIndex(0)
            navController.navigate(Screen.Home.route) {
                popUpTo(navController.graph.findStartDestination().id) {
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

    // SplashScreen에서 splashDone이 true가 되면 Home으로 navigate
    LaunchedEffect(viewModel.splashDone, currentRoute) {
        if (viewModel.splashDone && currentRoute == Screen.Splash.route) {
            navController.navigate(Screen.Home.route) {
                popUpTo(Screen.Splash.route) { inclusive = true }
            }
        }
    }

    // NavHost(=NavGraph)는 항상 마운트, SplashScreen은 NavGraph의 route로만 존재
    Scaffold(
        containerColor = Color(0xFFF5F5F5),
        contentWindowInsets = WindowInsets(0, 0, 0, 0),
        modifier = Modifier.padding(WindowInsets.systemBars.asPaddingValues()),
        topBar = {
            if (!(currentRoute.isEmpty() || currentRoute == Screen.Splash.route)
                && !currentRoute.startsWith("login")
                && !currentRoute.startsWith("signup")
                && !currentRoute.startsWith("findpw")
                && !currentRoute.startsWith("terms_agreement")
                && !currentRoute.startsWith(Screen.Setting.route)
                && !currentRoute.startsWith("chat")
            ) {
                SearchTopAppBar(
                    isSearchExpanded = isSearchExpanded,
                    searchText = searchText,
                    onSearchTextChange = { 
                        searchText = it
                        navController.currentBackStackEntry?.savedStateHandle?.set("searchQuery", it)
                    },
                    onBackClick = {
                        isSearchExpanded = false
                        navController.navigateUp()
                        searchText = ""
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
            if (!(currentRoute.isEmpty() || currentRoute == Screen.Splash.route)
                && !currentRoute.startsWith("chat")
                && !currentRoute.startsWith("login")
                && !currentRoute.startsWith("signup")
                && !currentRoute.startsWith("findpw")
                && !currentRoute.startsWith("terms_agreement")
            ) {
                Box(
                    modifier = Modifier
                        .graphicsLayer {
                            translationY = bottomBarOffset * 200f
                        }
                ) {
                    AnimatedBottomBar(
                        selectedIndex = selectedIndex,
                        onTabSelected = { index ->
                            viewModel.updateSelectedIndex(index)
                        },
                        navController = navController,
                        currentRoute = currentRoute
                    )
                }
            }
        }
    ) { innerPadding ->
        NavGraph(
            navController = navController,
            modifier = Modifier.padding(
                top = if (currentRoute == Screen.Home.route) 0.dp else innerPadding.calculateTopPadding(),
                bottom = innerPadding.calculateBottomPadding(),
                start = 0.dp,
                end = 0.dp
            ),
            viewModel = viewModel,
            authViewModel = authViewModel
        )
    }
}

@Composable
fun AnimatedBottomBar(
    selectedIndex: Int,
    onTabSelected: (Int) -> Unit,
    navController: NavHostController,
    currentRoute: String
) {
    val indicatorWidth = 60.dp
    val indicatorHeight = 64.dp

    BoxWithConstraints(
        modifier = Modifier
            .fillMaxWidth()
            .height(72.dp)
            .background(Color.White)
    ) {
        val itemWidth = maxWidth / 4
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
                Pair("랭킹", Icons.Filled.Star),
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
                                    ) {
                                        onTabSelected(index)
                                        // 탭 전환 시 해당 탭의 메인 화면으로 이동
                                        val target = when (index) {
                                            0 -> Screen.Home.route
                                            1 -> Screen.Stock.route
                                            2 -> Screen.Ranking.route
                                            3 -> Screen.MyPage.route
                                            else -> Screen.Home.route
                                        }
                                        navController.navigate(target) {
                                            // 시작 화면까지 모든 화면을 팝
                                            popUpTo(navController.graph.findStartDestination().id) {
                                                inclusive = false
                                            }
                                            // 같은 화면으로의 중복 네비게이션 방지
                                            launchSingleTop = true
                                            // 상태 복원 방지
                                            restoreState = false
                                        }
                                    }
                                } else {
                                    Modifier
                                }
                            ),
                        contentAlignment = Alignment.Center
                    ) {
                        Column(horizontalAlignment = Alignment.CenterHorizontally) {
                            Icon(
                                imageVector = icon,
                                contentDescription = label,
                                tint = iconColor,
                                modifier = Modifier.size(28.dp)
                            )
                            Text(
                                text = label,
                                fontSize = 12.sp,
                                color = iconColor,
                                fontFamily = SCDreamFontFamily
                            )
                        }
                    }
                }
            }
        }
    }
}