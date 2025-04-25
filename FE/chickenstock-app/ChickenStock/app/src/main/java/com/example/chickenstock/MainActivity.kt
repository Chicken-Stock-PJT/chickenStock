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

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        
        // 상태바 숨기기
        WindowCompat.setDecorFitsSystemWindows(window, false)
        
        enableEdgeToEdge()
        setContent {
            ChickenStockTheme {
                MainScreen()
            }
        }
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
    val iconOffsetX by animateDpAsState(
        targetValue = if (isSearchExpanded) (-160).dp else 0.dp,
        animationSpec = tween(300),
        label = "iconOffsetX"
    )

    val textFieldWidth by animateDpAsState(
        targetValue = if (isSearchExpanded) 200.dp else 0.dp,
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
                    IconButton(onClick = onBackClick) {
                        Icon(
                            imageVector = Icons.Default.ArrowBack,
                            contentDescription = "뒤로가기",
                            tint = Gray700,
                            modifier = Modifier.size(30.dp)
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
                    onClick = onSearchIconClick,
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
                        .offset(x = iconOffsetX + 40.dp)
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
                            .padding(horizontal = 12.dp),
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
fun MainScreen(viewModel: MainViewModel = viewModel()) {
    val navController = rememberNavController()
    val currentBackStack by navController.currentBackStackEntryAsState()
    val currentRoute = currentBackStack?.destination?.route ?: Screen.Home.route
    var searchText by remember { mutableStateOf("") }
    var isSearchExpanded by remember { mutableStateOf(false) }
    
    LaunchedEffect(currentRoute) {
        isSearchExpanded = currentRoute.startsWith("search")
    }

    val tabList = listOf(Screen.Home.route, Screen.Stock.route, Screen.MyPage.route)
    val selectedIndex by viewModel.selectedIndex
    val isBottomBarVisible by viewModel.isBottomBarVisible

    // 현재 라우트가 변경될 때 bottomBar 가시성 업데이트
    LaunchedEffect(currentRoute) {
        viewModel.setBottomBarVisibility(!currentRoute.startsWith("search"))
    }

    // 첫 실행 시 애니메이션을 위한 상태
    var isFirstLaunch by remember { mutableStateOf(true) }
    LaunchedEffect(Unit) {
        isFirstLaunch = false
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
            SearchTopAppBar(
                isSearchExpanded = isSearchExpanded,
                searchText = searchText,
                onSearchTextChange = { searchText = it },
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
                    isFirstLaunch = isFirstLaunch,
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
            viewModel = viewModel
        )
    }
}

@Composable
fun AnimatedBottomBar(
    selectedIndex: Int,
    isFirstLaunch: Boolean,
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
            animationSpec = tween(durationMillis = if (isFirstLaunch) 0 else 250),
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
                    animationSpec = tween(durationMillis = if (isFirstLaunch) 0 else 200),
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
                            .clickable(
                                indication = null,
                                interactionSource = remember { MutableInteractionSource() }
                            ) { onTabSelected(index) },
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