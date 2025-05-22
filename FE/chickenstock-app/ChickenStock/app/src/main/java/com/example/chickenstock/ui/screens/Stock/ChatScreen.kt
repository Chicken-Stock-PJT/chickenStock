package com.example.chickenstock.ui.screens.stock

import android.util.Log
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.ArrowBack
import androidx.compose.material.icons.filled.KeyboardArrowLeft
import androidx.compose.material.icons.filled.Send
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.Text
import androidx.compose.material3.TextField
import androidx.compose.material3.TextFieldDefaults
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import com.example.chickenstock.viewmodel.MainViewModel
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.activity.compose.LocalOnBackPressedDispatcherOwner
import androidx.navigation.NavController
import androidx.compose.ui.zIndex
import com.example.chickenstock.api.RetrofitClient
import com.example.chickenstock.api.StockService
import androidx.compose.ui.platform.LocalContext
import com.example.chickenstock.api.CommentResponse
import androidx.compose.material.icons.filled.Favorite
import com.example.chickenstock.data.TokenManager
import com.example.chickenstock.api.CommentPostRequest
import kotlinx.coroutines.launch
import com.example.chickenstock.viewmodel.AuthViewModel
import androidx.compose.ui.focus.onFocusChanged
import androidx.compose.ui.platform.LocalFocusManager
import androidx.compose.material3.AlertDialog
import androidx.compose.material3.TextButton
import com.example.chickenstock.ui.theme.SCDreamFontFamily
import androidx.compose.material.icons.filled.Edit
import androidx.compose.material.icons.filled.Delete
import androidx.compose.material.icons.filled.MoreVert
import androidx.compose.material3.DropdownMenu
import androidx.compose.material3.DropdownMenuItem
import androidx.compose.ui.unit.DpOffset
import androidx.compose.runtime.getValue
import androidx.compose.runtime.setValue
import androidx.compose.material.icons.filled.ChatBubble
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.ButtonDefaults
import com.example.chickenstock.api.CommentEditRequest
import android.content.Context
import kotlinx.coroutines.CoroutineScope
import androidx.compose.material3.IconButtonDefaults
import com.example.chickenstock.api.ReplyPostRequest
import androidx.compose.ui.focus.FocusManager
import androidx.compose.material.icons.outlined.FavoriteBorder

@Composable
fun ChatScreen(stockCode: String, viewModel: MainViewModel, navController: NavController, authViewModel: AuthViewModel) {
    val backDispatcher = LocalOnBackPressedDispatcherOwner.current?.onBackPressedDispatcher
    val context = LocalContext.current
    var stockName by remember { mutableStateOf("") }

    // 댓글 상태
    var comments by remember { mutableStateOf<List<CommentResponse>>(emptyList()) }
    var expandedCommentIds by remember { mutableStateOf(setOf<Int>()) }

    // 탑바/바텀바 숨기기
    LaunchedEffect(Unit) {
        viewModel.setTopBarVisibility(false)
        viewModel.setBottomBarVisibility(false)
    }
    DisposableEffect(Unit) {
        onDispose {
            viewModel.setTopBarVisibility(true)
            viewModel.setBottomBarVisibility(true)
        }
    }
    var chatInput by remember { mutableStateOf("") }
    val showLoginDialog = remember { mutableStateOf(false) }

    val focusManager = LocalFocusManager.current

    var deleteTargetCommentId by remember { mutableStateOf<Int?>(null) }
    var deleteTargetShortCode by remember { mutableStateOf<String?>(null) }
    var showDeleteDialog by remember { mutableStateOf(false) }

    // 종목명 API 호출
    LaunchedEffect(stockCode) {
        val stockService = RetrofitClient.getInstance(context).create(StockService::class.java)
        try {
            val response = stockService.getStockDetail(stockCode)
            Log.d("ChatScreen", "getStockDetail 응답: ${response.body()}")
            if (response.isSuccessful) {
                stockName = response.body()?.shortName ?: ""
            }
        } catch (_: Exception) {}
    }
    val coroutineScope = rememberCoroutineScope()
    // 댓글 목록 API 호출 함수
    fun refreshComments() {
        val stockService = RetrofitClient.getInstance(context).create(StockService::class.java)
        val refreshToken = TokenManager.getInstance(context).getRefreshToken()
        coroutineScope.launch {
            try {
                val response = if (!refreshToken.isNullOrBlank()) {
                    // 로그인 상태: 리프레시 토큰 포함
                    stockService.getCommentsWithAuth("Bearer $refreshToken", stockCode)
                } else {
                    // 비로그인: 기존 방식
                    stockService.getComments(stockCode)
                }
                if (response.isSuccessful) {
                    comments = response.body()?.comments ?: emptyList()
                }
            } catch (_: Exception) {}
        }
    }
    // 댓글 목록 최초 로딩
    LaunchedEffect(stockCode) {
        refreshComments()
    }

    Column(
        modifier = Modifier
            .fillMaxSize()
            .background(Color(0xFFF5F5F5))
    ) {
        // 상단 바 (고정)
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .padding(top = 24.dp, start = 12.dp, end = 8.dp)
                .zIndex(1f),
            verticalAlignment = Alignment.CenterVertically
        ) {
            IconButton(
                onClick = {
                    Log.d("ChatScreen", "뒤로가기 버튼 클릭")
                    if (backDispatcher != null) {
                        Log.d("ChatScreen", "backDispatcher.onBackPressed() 호출")
                        backDispatcher.onBackPressed()
                    } else {
                        Log.d("ChatScreen", "navController.navigateUp() 호출")
                        navController.navigateUp()
                    }
                },
                modifier = Modifier.size(48.dp)
            ) {
                Icon(
                    imageVector = Icons.Filled.KeyboardArrowLeft,
                    contentDescription = "뒤로가기",
                    tint = Color.Black,
                    modifier = Modifier.size(40.dp)
                )
            }
            Spacer(modifier = Modifier.width(8.dp))
            // 중앙에 종목명
            Box(modifier = Modifier.weight(1f), contentAlignment = Alignment.Center) {
                Text(
                    text = stockName,
                    color = Color.Black,
                    fontWeight = FontWeight.Bold,
                    fontSize = 18.sp,
                    maxLines = 1
                )
            }
            Spacer(modifier = Modifier.width(48.dp)) // 오른쪽 여백 확보
        }
        // 댓글 목록 (스크롤)
        LazyColumn(
            modifier = Modifier
                .weight(1f)
                .fillMaxWidth()
                .padding(bottom = 8.dp),
            contentPadding = PaddingValues(top = 12.dp, start = 12.dp, end = 12.dp)
        ) {
            items(comments) { comment ->
                CommentItem(
                    comment = comment,
                    expanded = expandedCommentIds.contains(comment.id),
                    onExpandClick = {
                        expandedCommentIds = if (expandedCommentIds.contains(comment.id))
                            expandedCommentIds - comment.id else expandedCommentIds + comment.id
                    },
                    myNickname = viewModel.userProfile.collectAsState().value?.nickname,
                    onDeleteClick = {
                        deleteTargetCommentId = comment.id
                        deleteTargetShortCode = stockCode
                        showDeleteDialog = true
                    },
                    context = context,
                    coroutineScope = coroutineScope,
                    stockCode = stockCode,
                    onCommentEdited = { refreshComments() },
                    onReplyPosted = { refreshComments() },
                    authViewModel = authViewModel,
                    focusManager = focusManager,
                    showLoginDialog = showLoginDialog
                )
            }
        }
        // 하단 고정 입력란 + 보내기 버튼 (고정)
        Box(
            modifier = Modifier
                .fillMaxWidth()
                .background(Color.White)
                .padding(start = 12.dp, end = 12.dp, bottom = 8.dp, top = 8.dp)
        ) {
            Row(
                modifier = Modifier.fillMaxWidth(),
                verticalAlignment = Alignment.CenterVertically
            ) {
                TextField(
                    value = chatInput,
                    onValueChange = {
                        if (authViewModel.isLoggedIn.value) {
                            chatInput = it
                        }
                    },
                    placeholder = { Text("댓글을 입력하세요") },
                    shape = RoundedCornerShape(20.dp),
                    colors = TextFieldDefaults.colors(
                        unfocusedContainerColor = Color(0xFFF5F5F5),
                        focusedContainerColor = Color(0xFFF5F5F5),
                        unfocusedIndicatorColor = Color.Transparent,
                        focusedIndicatorColor = Color.Transparent,
                        unfocusedTextColor = Color.Black,
                        focusedTextColor = Color.Black
                    ),
                    modifier = Modifier
                        .weight(1f)
                        .onFocusChanged { focusState ->
                            if (focusState.isFocused && !authViewModel.isLoggedIn.value) {
                                showLoginDialog.value = true
                                focusManager.clearFocus()
                            }
                        }
                )
                Spacer(modifier = Modifier.width(8.dp))
                Box(
                    modifier = Modifier
                        .size(44.dp)
                        .background(Color(0xFFFFEB3B), CircleShape)
                        .clickable(
                            interactionSource = remember { MutableInteractionSource() },
                            indication = null,
                            onClick = {
                                Log.d("ChatScreen", "보내기 버튼 클릭: isLoggedIn=${authViewModel.isLoggedIn.value}")
                                if (!authViewModel.isLoggedIn.value) {
                                    showLoginDialog.value = true
                                } else if (chatInput.isNotBlank()) {
                                    val stockService = RetrofitClient.getInstance(context).create(StockService::class.java)
                                    coroutineScope.launch {
                                        try {
                                            val token = TokenManager.getInstance(context).getAccessToken()
                                            val response = stockService.postComment(
                                                token = "Bearer $token",
                                                shortCode = stockCode,
                                                request = CommentPostRequest(content = chatInput)
                                            )
                                            if (response.isSuccessful) {
                                                val newComment = response.body()
                                                if (newComment != null) {
                                                    comments = listOf(newComment) + comments
                                                    chatInput = ""
                                                    refreshComments()
                                                }
                                            }
                                        } catch (_: Exception) {}
                                    }
                                }
                            }
                        ),
                    contentAlignment = Alignment.Center
                ) {
                    Icon(
                        imageVector = Icons.Filled.Send,
                        contentDescription = "보내기",
                        tint = Color.Black,
                        modifier = Modifier.size(24.dp)
                    )
                }
            }
        }
        // 로그인 필요 경고창
        if (showLoginDialog.value) {
            AlertDialog(
                onDismissRequest = { showLoginDialog.value = false },
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
                        "의견을 남기려면 로그인이 필요합니다.",
                        fontFamily = SCDreamFontFamily,
                        color = Color.Black
                    )
                },
                confirmButton = {
                    TextButton(
                        onClick = {
                            showLoginDialog.value = false
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
                        onClick = { showLoginDialog.value = false }
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
        // 삭제 경고 다이얼로그
        if (showDeleteDialog && deleteTargetCommentId != null && deleteTargetShortCode != null) {
            AlertDialog(
                onDismissRequest = { showDeleteDialog = false },
                title = { Text("댓글 삭제", fontFamily = SCDreamFontFamily, color = Color.Black, fontWeight = FontWeight.Bold) },
                text = { Text("정말 삭제하시겠습니까?", fontFamily = SCDreamFontFamily, color = Color.Black) },
                confirmButton = {
                    TextButton(
                        onClick = {
                            showDeleteDialog = false
                            val commentId = deleteTargetCommentId
                            val shortCode = deleteTargetShortCode
                            if (commentId != null && shortCode != null) {
                                val stockService = RetrofitClient.getInstance(context).create(StockService::class.java)
                                coroutineScope.launch {
                                    try {
                                        val token = TokenManager.getInstance(context).getAccessToken()
                                        val response = stockService.deleteComment(
                                            token = "Bearer $token",
                                            shortCode = shortCode,
                                            commentId = commentId
                                        )
                                        if (response.isSuccessful) {
                                            comments = comments.map {
                                                if (it.id == commentId) it.copy(deleted = true, content = null) else it
                                            }
                                            refreshComments()
                                        }
                                    } catch (_: Exception) {}
                                }
                            }
                            deleteTargetCommentId = null
                            deleteTargetShortCode = null
                        }
                    ) {
                        Text("삭제", color = Color.Red, fontFamily = SCDreamFontFamily, fontWeight = FontWeight.Bold)
                    }
                },
                dismissButton = {
                    TextButton(onClick = { showDeleteDialog = false }) {
                        Text("취소", color = Color(0xFF0066CC), fontFamily = SCDreamFontFamily, fontWeight = FontWeight.Bold)
                    }
                },
                containerColor = Color.White
            )
        }
    }
}

@Composable
fun CommentItem(
    comment: CommentResponse,
    expanded: Boolean,
    onExpandClick: () -> Unit,
    myNickname: String?,
    onDeleteClick: () -> Unit,
    context: Context,
    coroutineScope: CoroutineScope,
    stockCode: String,
    onCommentEdited: () -> Unit,
    onReplyPosted: () -> Unit,
    authViewModel: AuthViewModel,
    focusManager: FocusManager,
    showLoginDialog: MutableState<Boolean>
) {
    var showMenu by remember { mutableStateOf(false) }
    var isEditing by remember { mutableStateOf(false) }
    var editContent by remember { mutableStateOf(TextFieldValue(comment.content ?: "")) }
    Box(
        modifier = Modifier
            .fillMaxWidth()
            .padding(vertical = 8.dp)
            .background(Color.White, RoundedCornerShape(12.dp))
            .padding(16.dp)
    ) {
        Column(modifier = Modifier.fillMaxWidth()) {
            // 닉네임, 날짜 (항상 표시)
            Row(verticalAlignment = Alignment.CenterVertically, modifier = Modifier.fillMaxWidth()) {
                Row(verticalAlignment = Alignment.CenterVertically) {
                    Text(text = comment.nickname, fontWeight = FontWeight.Bold, color = Color.Black)
                    Spacer(modifier = Modifier.width(8.dp))
                    Text(text = comment.createdAt.take(16).replace("T", " "), color = Color.Gray, fontSize = 12.sp)
                }
                Spacer(modifier = Modifier.weight(1f))
            }
            if (isEditing) {
                // 수정 중일 때만 회색 네모 박스 + 입력창 + 버튼
                Box(
                    modifier = Modifier
                        .fillMaxWidth()
                        .background(Color(0xFFF5F5F5), RoundedCornerShape(10.dp))
                        .padding(12.dp)
                ) {
                    Column {
                        OutlinedTextField(
                            value = editContent,
                            onValueChange = { editContent = it },
                            modifier = Modifier.fillMaxWidth(),
                            textStyle = MaterialTheme.typography.bodyLarge.copy(color = Color.Black, fontSize = 15.sp),
                            singleLine = false,
                            maxLines = 5,
                            shape = RoundedCornerShape(8.dp),
                            colors = TextFieldDefaults.colors(
                                unfocusedContainerColor = Color(0xFFF5F5F5),
                                focusedContainerColor = Color(0xFFF5F5F5),
                                unfocusedIndicatorColor = Color.Transparent,
                                focusedIndicatorColor = Color.Transparent,
                                unfocusedTextColor = Color.Black,
                                focusedTextColor = Color.Black
                            )
                        )
                        Row(
                            modifier = Modifier.fillMaxWidth().padding(top = 8.dp),
                            horizontalArrangement = Arrangement.End
                        ) {
                            TextButton(
                                onClick = { isEditing = false },
                                colors = ButtonDefaults.textButtonColors(containerColor = Color.White)
                            ) {
                                Text("취소", color = Color.Gray, fontWeight = FontWeight.Bold)
                            }
                            Spacer(modifier = Modifier.width(8.dp))
                            TextButton(
                                onClick = {
                                    val token = TokenManager.getInstance(context).getAccessToken()
                                    val stockService = RetrofitClient.getInstance(context).create(StockService::class.java)
                                    coroutineScope.launch {
                                        try {
                                            val response = stockService.editComment(
                                                token = "Bearer $token",
                                                shortCode = stockCode,
                                                commentId = comment.id,
                                                request = CommentEditRequest(content = editContent.text)
                                            )
                                            if (response.isSuccessful) {
                                                val updated = response.body()
                                                if (updated != null) {
                                                    editContent = TextFieldValue(updated.content ?: "")
                                                    isEditing = false
                                                    onCommentEdited()
                                                }
                                            }
                                        } catch (_: Exception) {}
                                    }
                                },
                                colors = ButtonDefaults.textButtonColors(containerColor = Color(0xFF42A5F5))
                            ) {
                                Text("저장", color = Color.White, fontWeight = FontWeight.Bold)
                            }
                        }
                    }
                }
            } else {
                // 수정 중이 아닐 때만 컨텐츠, 하트, 대댓글, 옵션 표시
                if (comment.deleted) {
                    Text(text = "삭제된 댓글입니다", color = Color.Gray, fontSize = 14.sp, modifier = Modifier.padding(top = 4.dp))
                } else {
                    Text(text = comment.content ?: "", color = Color.Black, fontSize = 15.sp, modifier = Modifier.padding(top = 4.dp))
                }
                Row(verticalAlignment = Alignment.CenterVertically, modifier = Modifier.padding(top = 8.dp)) {
                    val isDeleted = comment.deleted
                    Icon(
                        imageVector = when {
                            isDeleted -> Icons.Outlined.FavoriteBorder
                            comment.likedByMe == true -> Icons.Filled.Favorite
                            else -> Icons.Outlined.FavoriteBorder
                        },
                        contentDescription = "좋아요",
                        tint = when {
                            isDeleted -> Color.LightGray
                            comment.likedByMe == true -> Color(0xFFFF0000)
                            else -> Color(0xFF888888)
                        },
                        modifier = Modifier
                            .size(18.dp)
                            .offset(y = 8.dp)
                            .let { m ->
                                if (isDeleted) m else m.clickable {
                                    if (!authViewModel.isLoggedIn.value) {
                                        showLoginDialog.value = true
                                        return@clickable
                                    }
                                    Log.d("ChatScreen", "댓글 좋아요 클릭: commentId=${comment.id}, likedByMe=${comment.likedByMe}")
                                    val token = TokenManager.getInstance(context).getAccessToken()
                                    val stockService = RetrofitClient.getInstance(context).create(StockService::class.java)
                                    coroutineScope.launch {
                                        try {
                                            val response = stockService.toggleLike(
                                                token = "Bearer $token",
                                                shortCode = stockCode,
                                                commentId = comment.id
                                            )
                                            if (response.isSuccessful) {
                                                onCommentEdited()
                                            }
                                        } catch (_: Exception) {}
                                    }
                                }
                            }
                    )
                    Spacer(modifier = Modifier.width(4.dp))
                    Text(
                        text = comment.likeCount.toString(),
                        color = Color.Gray,
                        fontSize = 13.sp,
                        modifier = Modifier.offset(y = 8.dp)
                    )
                    Spacer(modifier = Modifier.width(16.dp))
                    Icon(
                        imageVector = Icons.Filled.ChatBubble,
                        contentDescription = "대댓글",
                        tint = if (isDeleted) Color.LightGray else Color(0xFF2196F3),
                        modifier = Modifier
                            .size(18.dp)
                            .offset(y = 8.dp)
                            .clickable { onExpandClick() }
                    )
                    Spacer(modifier = Modifier.width(4.dp))
                    Text(
                        text = comment.children.count { !it.deleted }.toString(),
                        color = Color.Gray,
                        fontSize = 13.sp,
                        modifier = Modifier.offset(y = 8.dp)
                    )
                    Spacer(modifier = Modifier.weight(1f))
                    if (!myNickname.isNullOrBlank() && comment.nickname == myNickname && !comment.deleted && !isEditing) {
                        Box(
                            modifier = Modifier
                                .offset(x = 8.dp, y = 8.dp)
                        ) {
                            IconButton(onClick = { showMenu = true }) {
                                Icon(imageVector = Icons.Filled.MoreVert, contentDescription = "옵션", tint = Color.Gray, modifier = Modifier.size(20.dp))
                            }
                            DropdownMenu(
                                expanded = showMenu,
                                onDismissRequest = { showMenu = false },
                                offset = DpOffset(x = (-8).dp, y = 0.dp),
                                properties = androidx.compose.ui.window.PopupProperties(focusable = true),
                                containerColor = Color.White,
                                modifier = Modifier.wrapContentWidth()
                            ) {
                                DropdownMenuItem(
                                    text = { Text("수정", color = Color.Black) },
                                    onClick = {
                                        showMenu = false
                                        isEditing = true
                                        if (expanded) onExpandClick()
                                    }
                                )
                                DropdownMenuItem(
                                    text = { Text("삭제", color = Color.Black) },
                                    onClick = {
                                        showMenu = false
                                        onDeleteClick()
                                    }
                                )
                            }
                        }
                    }
                }
            }
            // 대댓글 펼치기
            if (expanded) {
                var chatReplyInput by remember { mutableStateOf("") }
                val hasReplies = comment.children.isNotEmpty()
                Column(
                    modifier = Modifier
                        .fillMaxWidth()
                        .background(Color.White)
                ) {
                    // 대댓글 리스트
                    if (hasReplies) {
                        comment.children.forEach { reply ->
                            var isReplyEditing by remember { mutableStateOf(false) }
                            var replyEditContent by remember { mutableStateOf(TextFieldValue(reply.content ?: "")) }
                            var showReplyMenu by remember { mutableStateOf(false) }
                            var showReplyDeleteDialog by remember { mutableStateOf(false) }
                            Box(
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .padding(vertical = 4.dp)
                                    .background(Color.White, RoundedCornerShape(8.dp))
                                    .padding(12.dp)
                            ) {
                                Column(modifier = Modifier.fillMaxWidth()) {
                                    Row(verticalAlignment = Alignment.CenterVertically, modifier = Modifier.fillMaxWidth()) {
                                        Text(text = reply.nickname, fontWeight = FontWeight.Bold, color = Color.Black, fontSize = 13.sp)
                                        Spacer(modifier = Modifier.width(8.dp))
                                        Text(text = reply.createdAt.take(16).replace("T", " "), color = Color.Gray, fontSize = 11.sp)
                                        Spacer(modifier = Modifier.weight(1f))
                                    }
                                    if (isReplyEditing) {
                                        // 대댓글 수정 인라인 UI
                                        Box(
                                            modifier = Modifier
                                                .fillMaxWidth()
                                                .background(Color(0xFFF5F5F5), RoundedCornerShape(10.dp))
                                                .padding(12.dp)
                                        ) {
                                            Column {
                                                OutlinedTextField(
                                                    value = replyEditContent,
                                                    onValueChange = { replyEditContent = it },
                                                    modifier = Modifier.fillMaxWidth(),
                                                    textStyle = MaterialTheme.typography.bodyLarge.copy(color = Color.Black, fontSize = 13.sp),
                                                    singleLine = false,
                                                    maxLines = 5,
                                                    shape = RoundedCornerShape(8.dp),
                                                    colors = TextFieldDefaults.colors(
                                                        unfocusedContainerColor = Color(0xFFF5F5F5),
                                                        focusedContainerColor = Color(0xFFF5F5F5),
                                                        unfocusedIndicatorColor = Color.Transparent,
                                                        focusedIndicatorColor = Color.Transparent,
                                                        unfocusedTextColor = Color.Black,
                                                        focusedTextColor = Color.Black
                                                    )
                                                )
                                                Row(
                                                    modifier = Modifier.fillMaxWidth().padding(top = 8.dp),
                                                    horizontalArrangement = Arrangement.End
                                                ) {
                                                    TextButton(onClick = { isReplyEditing = false }, colors = ButtonDefaults.textButtonColors(containerColor = Color.White)) {
                                                        Text("취소", color = Color.Gray, fontWeight = FontWeight.Bold)
                                                    }
                                                    Spacer(modifier = Modifier.width(8.dp))
                                                    TextButton(
                                                        onClick = {
                                                            val token = TokenManager.getInstance(context).getAccessToken()
                                                            val stockService = RetrofitClient.getInstance(context).create(StockService::class.java)
                                                            coroutineScope.launch {
                                                                try {
                                                                    val response = stockService.editComment(
                                                                        token = "Bearer $token",
                                                                        shortCode = stockCode,
                                                                        commentId = reply.id,
                                                                        request = CommentEditRequest(content = replyEditContent.text)
                                                                    )
                                                                    if (response.isSuccessful) {
                                                                        isReplyEditing = false
                                                                        onCommentEdited()
                                                                    }
                                                                } catch (_: Exception) {}
                                                            }
                                                        },
                                                        colors = ButtonDefaults.textButtonColors(containerColor = Color(0xFF42A5F5))
                                                    ) {
                                                        Text("저장", color = Color.White, fontWeight = FontWeight.Bold)
                                                    }
                                                }
                                            }
                                        }
                                    } else {
                                        if (reply.deleted) {
                                            Text(text = "삭제된 댓글입니다", color = Color.Gray, fontSize = 12.sp, modifier = Modifier.padding(top = 2.dp, bottom = 8.dp))
                                        } else {
                                            Text(text = reply.content ?: "", color = Color.Black, fontSize = 13.sp, modifier = Modifier.padding(top = 2.dp, bottom = 8.dp))
                                        }
                                        Row(verticalAlignment = Alignment.CenterVertically, modifier = Modifier.padding(top = 4.dp)) {
                                            Icon(
                                                imageVector = when {
                                                    reply.deleted -> Icons.Outlined.FavoriteBorder
                                                    reply.likedByMe == true -> Icons.Filled.Favorite
                                                    else -> Icons.Outlined.FavoriteBorder
                                                },
                                                contentDescription = "좋아요",
                                                tint = when {
                                                    reply.deleted -> Color.LightGray
                                                    reply.likedByMe == true -> Color(0xFFFF0000)
                                                    else -> Color(0xFF888888)
                                                },
                                                modifier = Modifier
                                                    .size(16.dp)
                                                    .offset(y = 4.dp)
                                                    .let { m ->
                                                        if (reply.deleted) m else m.clickable {
                                                            if (!authViewModel.isLoggedIn.value) {
                                                                showLoginDialog.value = true
                                                                return@clickable
                                                            }
                                                            Log.d("ChatScreen", "대댓글 좋아요 클릭: replyId=${reply.id}, likedByMe=${reply.likedByMe}")
                                                            val token = TokenManager.getInstance(context).getAccessToken()
                                                            val stockService = RetrofitClient.getInstance(context).create(StockService::class.java)
                                                            coroutineScope.launch {
                                                                try {
                                                                    val response = stockService.toggleLike(
                                                                        token = "Bearer $token",
                                                                        shortCode = stockCode,
                                                                        commentId = reply.id
                                                                    )
                                                                    if (response.isSuccessful) {
                                                                        onCommentEdited()
                                                                    }
                                                                } catch (_: Exception) {}
                                                            }
                                                        }
                                                    }
                                            )
                                            Spacer(modifier = Modifier.width(4.dp))
                                            Text(
                                                text = reply.likeCount.toString(),
                                                color = Color.Gray,
                                                fontSize = 12.sp,
                                                modifier = Modifier.offset(y = 4.dp)
                                            )
                                            Spacer(modifier = Modifier.weight(1f))
                                            if (!myNickname.isNullOrBlank() && reply.nickname == myNickname && !reply.deleted) {
                                                Box {
                                                    IconButton(onClick = { showReplyMenu = true }, modifier = Modifier.size(18.dp)) {
                                                        Icon(imageVector = Icons.Filled.MoreVert, contentDescription = "옵션", tint = Color.Gray)
                                                    }
                                                    DropdownMenu(
                                                        expanded = showReplyMenu,
                                                        onDismissRequest = { showReplyMenu = false },
                                                        offset = DpOffset(x = (-8).dp, y = 0.dp),
                                                        properties = androidx.compose.ui.window.PopupProperties(focusable = true),
                                                        containerColor = Color.White,
                                                        modifier = Modifier.wrapContentWidth()
                                                    ) {
                                                        DropdownMenuItem(
                                                            text = { Text("수정", color = Color.Black) },
                                                            onClick = {
                                                                isReplyEditing = true
                                                                showReplyMenu = false
                                                            }
                                                        )
                                                        DropdownMenuItem(
                                                            text = { Text("삭제", color = Color.Black) },
                                                            onClick = {
                                                                showReplyDeleteDialog = true
                                                                showReplyMenu = false
                                                            }
                                                        )
                                                    }
                                                }
                                                // 삭제 경고 다이얼로그
                                                if (showReplyDeleteDialog) {
                                                    AlertDialog(
                                                        onDismissRequest = { showReplyDeleteDialog = false },
                                                        title = { Text("댓글 삭제", fontFamily = SCDreamFontFamily, color = Color.Black, fontWeight = FontWeight.Bold) },
                                                        text = { Text("정말 삭제하시겠습니까?", fontFamily = SCDreamFontFamily, color = Color.Black) },
                                                        confirmButton = {
                                                            TextButton(
                                                                onClick = {
                                                                    showReplyDeleteDialog = false
                                                                    val token = TokenManager.getInstance(context).getAccessToken()
                                                                    val stockService = RetrofitClient.getInstance(context).create(StockService::class.java)
                                                                    coroutineScope.launch {
                                                                        try {
                                                                            val response = stockService.deleteComment(
                                                                                token = "Bearer $token",
                                                                                shortCode = stockCode,
                                                                                commentId = reply.id
                                                                            )
                                                                            if (response.isSuccessful) {
                                                                                onCommentEdited()
                                                                            }
                                                                        } catch (_: Exception) {}
                                                                    }
                                                                }
                                                            ) {
                                                                Text("삭제", color = Color.Red, fontFamily = SCDreamFontFamily, fontWeight = FontWeight.Bold)
                                                            }
                                                        },
                                                        dismissButton = {
                                                            TextButton(onClick = { showReplyDeleteDialog = false }) {
                                                                Text("취소", color = Color(0xFF0066CC), fontFamily = SCDreamFontFamily, fontWeight = FontWeight.Bold)
                                                            }
                                                        },
                                                        containerColor = Color.White
                                                    )
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        Text(
                            text = "대댓글이 없습니다.",
                            color = Color.Gray,
                            fontSize = 13.sp,
                            modifier = Modifier.padding(8.dp)
                        )
                    }
                    // 삭제된 댓글이 아니면 대댓글 입력란 표시
                    if (!comment.deleted) {
                        val isLoggedIn = authViewModel.isLoggedIn.value
                        Row(
                            modifier = Modifier.fillMaxWidth().padding(top = 4.dp),
                            verticalAlignment = Alignment.CenterVertically
                        ) {
                            OutlinedTextField(
                                value = chatReplyInput,
                                onValueChange = {
                                    if (isLoggedIn) chatReplyInput = it
                                },
                                modifier = Modifier.weight(1f)
                                    .onFocusChanged { focusState ->
                                        if (focusState.isFocused && !isLoggedIn) {
                                            showLoginDialog.value = true
                                            focusManager.clearFocus()
                                        }
                                    },
                                placeholder = { Text("대댓글을 입력하세요", color = Color.Gray) },
                                singleLine = true,
                                shape = RoundedCornerShape(20.dp),
                                colors = TextFieldDefaults.colors(
                                    unfocusedContainerColor = Color(0xFFF5F5F5),
                                    focusedContainerColor = Color(0xFFF5F5F5),
                                    unfocusedIndicatorColor = Color.Transparent,
                                    focusedIndicatorColor = Color.Transparent,
                                    unfocusedTextColor = Color.Black,
                                    focusedTextColor = Color.Black
                                )
                            )
                            Spacer(modifier = Modifier.width(8.dp))
                            IconButton(
                                onClick = {
                                    if (!isLoggedIn) {
                                        showLoginDialog.value = true
                                    } else if (chatReplyInput.isNotBlank()) {
                                        val token = TokenManager.getInstance(context).getAccessToken()
                                        val stockService = RetrofitClient.getInstance(context).create(StockService::class.java)
                                        coroutineScope.launch {
                                            try {
                                                val response = stockService.postReply(
                                                    token = "Bearer $token",
                                                    shortCode = stockCode,
                                                    request = ReplyPostRequest(content = chatReplyInput, parentId = comment.id)
                                                )
                                                if (response.isSuccessful) {
                                                    chatReplyInput = ""
                                                    onReplyPosted()
                                                }
                                            } catch (_: Exception) {}
                                        }
                                    }
                                },
                                colors = IconButtonDefaults.iconButtonColors(containerColor = Color(0xFFFFEB3B)),
                                enabled = isLoggedIn
                            ) {
                                Icon(imageVector = Icons.Filled.Send, contentDescription = "대댓글 작성", tint = Color.Black, modifier = Modifier.size(22.dp))
                            }
                        }
                    }
                }
            }
        }
    }
} 