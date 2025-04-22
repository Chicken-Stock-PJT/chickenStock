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
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp
import androidx.navigation.compose.*
import com.example.chickenstock.ui.theme.ChickenStockTheme
import com.example.chickenstock.ui.screens.HomeScreen
import com.example.chickenstock.ui.screens.MyPageScreen
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.runtime.remember
import androidx.compose.ui.res.painterResource
import com.example.chickenstock.R

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
fun MainScreen() {
    val navController = rememberNavController()
    val currentBackStack by navController.currentBackStackEntryAsState()
    val currentRoute = currentBackStack?.destination?.route ?: "home"

    val tabList = listOf("home", "mypage")
    val selectedIndex = tabList.indexOf(currentRoute).coerceAtLeast(0)

    Scaffold(
        topBar = {
            TopAppBar(
                title = {},
                navigationIcon = {
                    Icon(
                        painter = painterResource(id = R.drawable.logo),
                        contentDescription = "Logo",
                        tint = Color.Unspecified
                    )
                },
                actions = {
                    IconButton(onClick = { /* 검색 기능 추가 */ }) {
                        Icon(Icons.Filled.Search, contentDescription = "Search")
                    }
                }
            )
        },
        bottomBar = {
            AnimatedBottomBar(
                selectedIndex = selectedIndex,
                onTabSelected = { index ->
                    val target = tabList[index]
                    navController.navigate(target) {
                        launchSingleTop = true
                        restoreState = true
                        popUpTo(navController.graph.startDestinationId) {
                            saveState = true
                        }
                    }
                }
            )
        }
    ) { innerPadding ->
        NavHost(
            navController = navController,
            startDestination = "home",
            modifier = Modifier.padding(innerPadding)
        ) {
            composable("home") { HomeScreen() }
            composable("mypage") { MyPageScreen() }
        }
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
        val itemWidth = maxWidth / 2
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
                Pair("내 기록", Icons.Filled.Person)
            ).forEachIndexed { index, (label, icon) ->
                val isSelected = index == selectedIndex
                val iconColor by animateColorAsState(
                    targetValue = if (isSelected) Color.Red else Color.Black,
                    animationSpec = tween(durationMillis = 200, delayMillis = 200)
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
                            Text(label, color = iconColor)
                        }
                    }
                }
            }
        }
    }
}