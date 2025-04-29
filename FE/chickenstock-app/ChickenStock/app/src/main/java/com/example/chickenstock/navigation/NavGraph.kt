package com.example.chickenstock.navigation

import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.navigation.NavHostController
import androidx.navigation.NavType
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.navArgument
import com.example.chickenstock.ui.screens.stock.StockScreen
import com.example.chickenstock.ui.screens.home.HomeScreen
import com.example.chickenstock.ui.screens.mypage.MyPageScreen
import com.example.chickenstock.viewmodel.MainViewModel
import com.example.chickenstock.ui.screens.stock.StockDetailScreen
import com.example.chickenstock.ui.components.StockItem
import com.example.chickenstock.ui.screens.search.SearchScreen
import com.example.chickenstock.ui.screens.login.LoginScreen
import com.example.chickenstock.ui.screens.login.SignupScreen
import com.example.chickenstock.ui.screens.mypage.SettingScreen
import com.example.chickenstock.viewmodel.AuthViewModel
import androidx.compose.ui.platform.LocalContext
import androidx.lifecycle.viewmodel.compose.viewModel
import com.example.chickenstock.ui.screens.login.FindPWScreen
import com.example.chickenstock.ui.screens.login.VerificationScreen
import com.example.chickenstock.ui.screens.login.SignupSuccessScreen

sealed class Screen(val route: String) {
    object Home : Screen("home")
    object Stock : Screen("stock")
    object MyPage : Screen("mypage")
    object Search : Screen("search")
    object Login : Screen("login")
    object Signup : Screen("signup")
    object Setting : Screen("setting")
    object FindPW : Screen("findpw")
    object Verification : Screen("verification")
    object SignupSuccess : Screen("signup_success")
    object StockDetail : Screen("stock_detail/{stockCode}") {
        fun createRoute(stockCode: String) = "stock_detail/$stockCode"
    }
}

@Composable
fun NavGraph(
    navController: NavHostController,
    modifier: Modifier = Modifier,
    viewModel: MainViewModel,
    authViewModel: AuthViewModel = viewModel(factory = AuthViewModel.Factory(LocalContext.current))
) {
    NavHost(
        navController = navController,
        startDestination = Screen.Home.route,
        modifier = modifier
    ) {
        composable(Screen.Home.route) {
            HomeScreen(navController = navController, viewModel = viewModel, authViewModel = authViewModel)
        }
        composable(Screen.Stock.route) {
            StockScreen(navController = navController, authViewModel = authViewModel)
        }
        composable(
            route = Screen.MyPage.route + "?tab={tab}",
            arguments = listOf(
                navArgument("tab") {
                    type = NavType.StringType
                    nullable = true
                }
            )
        ) { entry ->
            val tab = entry.arguments?.getString("tab") ?: "portfolio"
            MyPageScreen(
                navController = navController,
                authViewModel = authViewModel,
                initialTab = tab
            )
        }
        composable(Screen.Search.route) {
            SearchScreen(navController = navController)
        }
        composable(Screen.Login.route) {
            LoginScreen(navController = navController, authViewModel = authViewModel)
        }
        composable(Screen.Signup.route) {
            SignupScreen(navController = navController)
        }
        composable(Screen.Setting.route) {
            SettingScreen(
                navController = navController,
                viewModel = viewModel,
                authViewModel = authViewModel
            )
        }
        composable(Screen.FindPW.route) {
            FindPWScreen(navController = navController)
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
        composable(
            route = "verification?email={email}&name={name}&nickname={nickname}&password={password}",
            arguments = listOf(
                navArgument("email") { type = NavType.StringType },
                navArgument("name") { type = NavType.StringType },
                navArgument("nickname") { type = NavType.StringType },
                navArgument("password") { type = NavType.StringType }
            )
        ) { entry ->
            VerificationScreen(
                navController = navController,
                email = entry.arguments?.getString("email") ?: "",
                name = entry.arguments?.getString("name") ?: "",
                nickname = entry.arguments?.getString("nickname") ?: "",
                password = entry.arguments?.getString("password") ?: "",
                viewModel = viewModel
            )
        }
        composable(Screen.SignupSuccess.route) {
            SignupSuccessScreen(
                navController = navController,
                viewModel = viewModel
            )
        }
    }
} 