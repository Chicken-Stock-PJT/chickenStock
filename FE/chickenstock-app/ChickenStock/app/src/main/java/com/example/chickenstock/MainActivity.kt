package com.example.chickenstock

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.enableEdgeToEdge
import androidx.compose.animation.core.animateDpAsState
import androidx.compose.animation.core.tween
import androidx.compose.animation.animateColorAsState
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Home
import androidx.compose.material.icons.filled.Person
import androidx.compose.material.icons.filled.Search
import androidx.compose.material.icons.filled.ShowChart
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.navigation.compose.*
import com.example.chickenstock.ui.theme.ChickenStockTheme
import com.example.chickenstock.navigation.NavGraph
import com.example.chickenstock.navigation.Screen
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.runtime.remember
import androidx.compose.ui.res.painterResource
import androidx.lifecycle.ViewModel
import androidx.lifecycle.viewmodel.compose.viewModel
import com.example.chickenstock.R
import com.example.chickenstock.ui.theme.Gray0
import com.example.chickenstock.ui.theme.Gray700
import com.example.chickenstock.ui.theme.SCDreamFontFamily

class MainViewModel : ViewModel() {
    private val _selectedIndex = mutableStateOf(0)
    val selectedIndex: State<Int> = _selectedIndex

    fun updateSelectedIndex(index: Int) {
        _selectedIndex.value = index
    }
}

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        enableEdgeToEdge()
        setContent {
            ChickenStockTheme {
                MainScreen()
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

    val tabList = listOf(Screen.Home.route, Screen.Stock.route, Screen.MyPage.route)
    val selectedIndex by viewModel.selectedIndex

    // 현재 라우트가 변경될 때 selectedIndex 업데이트
    LaunchedEffect(currentRoute) {
        // 상세 페이지인 경우 이전 라우트를 확인
        val route = if (currentRoute.startsWith("stock_detail")) {
            navController.previousBackStackEntry?.destination?.route ?: Screen.Home.route
        } else {
            currentRoute
        }
        val index = tabList.indexOf(route).coerceAtLeast(0)
        viewModel.updateSelectedIndex(index)
    }

    // 첫 실행 시 애니메이션을 위한 상태
    var isFirstLaunch by remember { mutableStateOf(true) }
    LaunchedEffect(Unit) {
        isFirstLaunch = false
    }

    Scaffold(
        containerColor = Gray0,
        topBar = {
            TopAppBar(
                title = {},
                navigationIcon = {
                    Icon(
                        painter = painterResource(id = R.drawable.logo),
                        contentDescription = "Logo",
                        tint = Color.Unspecified,
                        modifier = Modifier
                            .size(52.dp)
                            .padding(start = 8.dp)
                    )
                },
                actions = {
                    IconButton(onClick = { /* 검색 기능 추가 */ }) {
                        Icon(
                            Icons.Filled.Search,
                            contentDescription = "Search",
                            tint = Gray700,
                            modifier = Modifier.size(30.dp)
                        )
                    }
                },
                colors = TopAppBarDefaults.topAppBarColors(
                    containerColor = Gray0
                )
            )
        },
        bottomBar = {
            AnimatedBottomBar(
                selectedIndex = selectedIndex,
                isFirstLaunch = isFirstLaunch,
                onTabSelected = { index ->
                    val target = tabList[index]
                    // 현재 라우트가 상세 페이지인 경우
                    if (currentRoute.startsWith("stock_detail")) {
                        // 이전 라우트를 확인
                        val previousRoute = navController.previousBackStackEntry?.destination?.route ?: Screen.Home.route
                        // 이전 라우트가 목표 라우트와 다른 경우에만 네비게이션
                        if (previousRoute != target) {
                            navController.navigate(target) {
                                launchSingleTop = true
                                popUpTo(navController.graph.startDestinationId) {
                                    saveState = true
                                }
                            }
                        }
                    } else {
                        // 일반적인 탭 전환
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