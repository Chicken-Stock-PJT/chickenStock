package com.example.chickenstock.navigation

import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.navigation.NavHostController
import androidx.navigation.NavType
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.navArgument
import com.example.chickenstock.ui.screens.Stock.StockScreen
import com.example.chickenstock.ui.screens.Home.HomeScreen
import com.example.chickenstock.ui.screens.MyPage.MyPageScreen
import com.example.chickenstock.MainViewModel
import com.example.chickenstock.ui.screens.stock.StockDetailScreen
import com.example.chickenstock.ui.components.StockItem

sealed class Screen(val route: String) {
    object Home : Screen("home")
    object Stock : Screen("stock")
    object MyPage : Screen("mypage")
    object StockDetail : Screen("stock_detail/{stockCode}") {
        fun createRoute(stockCode: String) = "stock_detail/$stockCode"
    }
}

@Composable
fun NavGraph(
    navController: NavHostController,
    modifier: Modifier = Modifier,
    viewModel: MainViewModel
) {
    NavHost(
        navController = navController,
        startDestination = Screen.Home.route,
        modifier = modifier
    ) {
        composable(Screen.Home.route) {
            HomeScreen(navController, viewModel)
        }
        composable(Screen.Stock.route) {
            StockScreen(stockId = "all", navController = navController)
        }
        composable(Screen.MyPage.route) {
            MyPageScreen()
        }
        composable(
            route = Screen.StockDetail.route,
            arguments = listOf(
                navArgument("stockCode") { type = NavType.StringType }
            )
        ) { backStackEntry ->
            val stockCode = backStackEntry.arguments?.getString("stockCode") ?: ""
            // 임시 데이터 생성
            val stock = StockItem(
                stockCode = stockCode,
                stockName = "삼성전자",
                market = "KOSPI",
                currentPrice = "73200",
                fluctuationRate = "-0.40",
                tradeAmount = "950"
            )
            StockDetailScreen(navController = navController, stock = stock)
        }
    }
} 